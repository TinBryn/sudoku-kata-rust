use std::slice::Iter;

use rand::prelude::ThreadRng;
use rand::Rng;


#[derive(Clone)]
pub struct Board {
    pub state: Vec<usize>
}

impl Board {
    pub fn new() -> Self {
        Board { state: vec![0; 81] }
    }

    pub fn iter(&self) -> Iter<'_, usize> {
        self.state.iter()
    }

    pub fn state_to_printable_board(state: &Board) -> String {
        let line = "+---+---+---+";
        let middle = "|...|...|...|";
        let mut board: Vec<Vec<char>> = vec![
            line.chars().collect(),
            middle.chars().collect(),
            middle.chars().collect(),
            middle.chars().collect(),
            line.chars().collect(),
            middle.chars().collect(),
            middle.chars().collect(),
            middle.chars().collect(),
            line.chars().collect(),
            middle.chars().collect(),
            middle.chars().collect(),
            middle.chars().collect(),
            line.chars().collect(),
        ];

        for row in 0..9 {
            for col in 0..9 {
                let row_to_write = row + row / 3 + 1;
                let col_to_write = col + col / 3 + 1;

                let digit = state[row + 9 * col];

                if digit == 0 {
                    continue;
                }

                board[row_to_write][col_to_write] = ('0' as usize + digit) as u8 as char;
            }
        }

        let board_argument = &board;
        let lines = board_argument.iter().map(|v| v.iter().collect::<String>());

        let mut result = lines.fold(String::new(), |acc, line| {
            let mut acc = acc;
            acc.push('\n');
            acc.push_str(line.as_str());
            acc
        });

        result.push('\n');

        result
    }

    pub fn code(&self) -> String {
        self.iter().map(|i| i.to_string()).collect()
    }
}


impl std::ops::Index<usize> for Board {
    type Output = usize;

    fn index(&self, index: usize) -> &Self::Output {
        &self.state[index]
    }
}

impl std::ops::IndexMut<usize> for Board {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.state[index]
    }
}

impl Default for Board {
    fn default() -> Self {
        Board::new()
    }
}

#[derive(Default)]
pub struct Stacks {
    state: Vec<State>,
}

impl Stacks {
    pub fn new() -> Self {
        Stacks {
            state: Vec::default(),
        }
    }

    pub fn len(&self) -> usize {
        self.state.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&mut self, state: State) {
        self.state.push(state);
    }

    pub fn pop(&mut self) -> Option<State> {
        self.state.pop()
    }

    pub fn last(&self) -> Option<&State> {
        self.state.last()
    }

    pub fn last_mut(&mut self) -> Option<&mut State> {
        self.state.last_mut()
    }

    pub fn do_move(&mut self) -> Commands {
        let State { state: current_state, row_index: row_to_move, col_index: col_to_move, used_digits, last_digit: digit_to_move } = self.last_mut().unwrap();

        let current_state_index = 9 * *row_to_move + *col_to_move;

        let mut moved_to_digit = *digit_to_move + 1;
        while moved_to_digit <= 9 && used_digits.as_mut().unwrap()[moved_to_digit - 1] {
            moved_to_digit += 1;
        }

        if *digit_to_move > 0 {
            used_digits.as_mut().unwrap()[*digit_to_move - 1] = false;
            current_state[current_state_index as usize] = 0;
        }

        if moved_to_digit <= 9 {
            *digit_to_move = moved_to_digit;
            used_digits.as_mut().unwrap()[moved_to_digit - 1] = true;
            current_state[current_state_index as usize] = moved_to_digit;

            // Next possible digit was found at current position
            // Next step will be to expand the state
            Commands::Expand
        } else {
            // No viable candidate was found at current position - pop it in the next iteration
            *digit_to_move = 0;
            Commands::Collapse
        }
    }

    pub fn do_move_2(&mut self) -> Commands {
        let State { state: current_state, row_index: row_to_move, col_index: col_to_move, used_digits, last_digit: digit_to_move } = self.last_mut().unwrap();
        let current_state_index = 9 * *row_to_move + *col_to_move;

        let mut moved_to_digit = *digit_to_move + 1;
        while moved_to_digit <= 9 && used_digits.as_mut().unwrap()[moved_to_digit as usize - 1] {
            moved_to_digit += 1;
        }

        if *digit_to_move > 0 {
            used_digits.as_mut().unwrap()[*digit_to_move - 1] = false;
            current_state[current_state_index as usize] = 0;
        }

        if moved_to_digit <= 9 {
            *digit_to_move = moved_to_digit;
            used_digits.as_mut().unwrap()[moved_to_digit - 1] = true;
            current_state[current_state_index as usize] = moved_to_digit;

            if current_state.iter().any(|digit| *digit == 0) {
                Commands::Expand
            } else {
                Commands::Complete
            }
        } else {
            // No viable candidate was found at current position - pop it in the next iteration
            *digit_to_move = 0;
            Commands::Collapse
        }
    }

    pub fn do_collapse(&mut self) -> Commands {
        self.pop();

        Commands::Move // Always try to move after collapse
    }

    fn check_digits_used(current_state: &Board, row: usize, col: usize, block_row: usize, block_col: usize) -> Vec<bool> {
        let mut is_digit_used = vec![false; 9];

        for i in 0..9 {
            let row_digit = current_state[9 * i + col];
            if row_digit > 0 {
                is_digit_used[row_digit - 1] = true;
            }

            let col_digit = current_state[9 * row + i];
            if col_digit > 0 {
                is_digit_used[col_digit - 1] = true;
            }

            let block_digit =
                current_state[(block_row * 3 + i / 3) * 9 + (block_col * 3 + i % 3)];
            if block_digit > 0 {
                is_digit_used[block_digit - 1] = true;
            }
        }
        is_digit_used
    }

    pub fn do_expand(&mut self, rng: &mut ThreadRng, alternate_state: &Board) -> Commands {
        let current_state = match self.last() {
            Some(state) => state.state.clone(),
            None => alternate_state.clone()
        };

        let mut best_row: i32 = -1;
        let mut best_col: i32 = -1;
        let mut best_used_digits = None;
        let mut best_candidates_count: i32 = -1;
        let mut best_random_value: i32 = -1;
        let mut contains_unsolvable_cells = false;

        for index in 0..81 {
            if current_state[index] == 0 {
                let row = index / 9;
                let col = index % 9;
                let block_row = row / 3;
                let block_col = col / 3;

                let is_digit_used = Stacks::check_digits_used(&current_state, row, col, block_row, block_col);

                let candidates_count = is_digit_used
                    .iter()
                    .filter(|used| !**used)
                    .count() as i32;

                if candidates_count == 0 {
                    contains_unsolvable_cells = true;
                    break;
                }

                let random_value = rng.gen_range(0..i32::MAX); // .Next();

                if best_candidates_count < 0
                    || candidates_count < best_candidates_count
                    || (candidates_count == best_candidates_count
                    && random_value < best_random_value)
                {
                    best_row = row as i32;
                    best_col = col as i32;
                    best_used_digits = Some(is_digit_used);
                    best_candidates_count = candidates_count;
                    best_random_value = random_value;
                }
            } // for (index = 0..81)
        }

        if !contains_unsolvable_cells {
            self.push(State {
                state: current_state,
                row_index: best_row,
                col_index: best_col,
                used_digits: best_used_digits,
                last_digit: 0
            });
        }

        // Always try to move after expand
        Commands::Move
    }
}

#[derive(Clone)]
pub struct State {
    pub state: Board,
    row_index: i32,
    col_index: i32,
    used_digits: Option<Vec<bool>>,
    last_digit: usize,
}


#[derive(PartialEq)]
pub enum Commands {
    Expand,
    Collapse,
    Move,
    Complete,
    Fail,
}
