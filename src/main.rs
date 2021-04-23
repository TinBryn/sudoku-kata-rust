mod sudoku_kata {
    use std::collections::BTreeSet;
    use std::collections::VecDeque;

    use rand::Rng;

    use sudoku_refactor::{CellGroup, List, NMaskGroups, TwoDigitMask, LookupStructures};
    use sudoku_refactor::{Board, Commands, Stacks};

    pub(crate) struct Program;


    impl Program {
        pub fn play() {
            let mut rng = rand::thread_rng();

            // #region Construct fully populated board

            // Construct board to be solved
            let mut stacks = Stacks::new();

            // Indicates operation to perform next
            // - expand - finds next empty cell and puts new state on stacks
            // - move - finds next candidate number at current pos and applies it to current state
            // - collapse - pops current state from stack as it did not yield a solution
            let mut command = Commands::Expand;
            while stacks.len() <= 9 * 9 {
                command = match command {
                    Commands::Expand => stacks.do_expand(&mut rng, &Board::default()),
                    Commands::Collapse => stacks.do_collapse(),
                    Commands::Move => stacks.do_move(),
                    _ => command,
                };
            }
            let state = &mut stacks.last_mut().unwrap().state;

            println!("\nFinal look of the solved board:");
            println!("{}", Board::state_to_printable_board(&state));
            // #endregion

            // #region Generate initial board from the completely solved one
            // Board is solved at this point.
            // Now pick subset of digits as the starting position.
            let remaining_digits = 30;
            let max_removed_per_block = 6;
            let mut removed_per_block = vec![vec![0; 3]; 3];
            let mut positions: Vec<usize> = (0..81).into_iter().collect();


            let final_state = state.clone();

            let mut removed_pos = 0;
            while removed_pos < 81 - remaining_digits {
                let cur_remaining_digits = 81 - removed_pos;
                let index_to_pick = removed_pos + rng.gen_range(0..cur_remaining_digits); // .Next(curRemainingDigits);

                let row = positions[index_to_pick] / 9;
                let col = positions[index_to_pick] % 9;

                let block_row_to_remove = row / 3;
                let block_col_to_remove = col / 3;

                if removed_per_block[block_row_to_remove][block_col_to_remove] >= max_removed_per_block {
                    continue;
                }

                removed_per_block[block_row_to_remove][block_col_to_remove] += 1;

                positions.swap(removed_pos, index_to_pick);

                let state_index = 9 * row + col;
                state[state_index] = 0;

                removed_pos += 1;
            }

            println!();
            println!("Starting look of the board to solve:");
            println!("{}", Board::state_to_printable_board(&state));
            // #endregion

            println!();
            println!();
            println!();

            // #region Prepare lookup structures that will be used in further execution

            let lookups = LookupStructures::new();
            // #endregion

            let mut change_made = true;
            while change_made {
                change_made = false;

                // #region Calculate candidates for current state of the board
                let mut candidate_masks = vec![0; 81];

                for i in 0..81 {
                    if state[i] == 0 {
                        let row = i / 9;
                        let col = i % 9;
                        let block_row = row / 3;
                        let block_col = col / 3;

                        let mut colliding_numbers = 0;
                        for j in 0..9 {
                            let row_sibling_index = 9 * row + j;
                            let col_sibling_index = 9 * j + col;
                            let block_sibling_index =
                                9 * (block_row * 3 + j / 3) + block_col * 3 + j % 3;

                            let shift = state[row_sibling_index] as i32 - 1;
                            let row_sibling_mask = if shift >= 0 { 1 << shift } else { 0 };
                            let shift = state[col_sibling_index] as i32 - 1;
                            let col_sibling_mask = if shift >= 0 { 1 << shift } else { 0 };
                            let shift = state[block_sibling_index] as i32 - 1;
                            let block_sibling_mask = if shift >= 0 { 1 << shift } else { 0 };

                            colliding_numbers = colliding_numbers
                                | row_sibling_mask
                                | col_sibling_mask
                                | block_sibling_mask;
                        }

                        candidate_masks[i] = lookups.all_ones() & !colliding_numbers;
                    }
                }
                // #endregion

                // #region Build a collection (named cellGroups) which maps cell indices into distinct groups (rows/columns/blocks)
                let rows_indices = (0..81).into_iter().map(|index| CellGroup {
                    discriminator: index / 9,
                    description: format!("row #{}", index / 9 + 1),
                    index,
                    row: index / 9,
                    col: index % 9,
                });

                let column_indices = (0..81).into_iter().map(|index| CellGroup {
                    discriminator: 9 + index % 9,
                    description: format!("column #{}", index % 9 + 1),
                    index,
                    row: index / 9,
                    col: index % 9,
                });

                let block_indices = (0..81).into_iter().map(|index| {
                    let row = index / 9;
                    let col = index % 9;
                    CellGroup {
                        discriminator: 18 + 3 * (row / 3) + col / 3,
                        description: format!("block ({}, {})", row / 3 + 1, col / 3 + 1),
                        index,
                        row,
                        col,
                    }
                });

                let cell_groups = rows_indices
                    .chain(column_indices)
                    .chain(block_indices);

                let cell_groups = sudoku_refactor::group_iter_by_key(cell_groups, |tuple| tuple.discriminator);
                // #endregion

                let mut step_change_made = true;
                while step_change_made {
                    step_change_made = false;

                    // #region Pick cells with only one candidate left

                    let single_candidate_indices: Vec<_> = candidate_masks
                        .iter()
                        .enumerate()
                        .map(|(index, mask)| (index, lookups.mask_to_ones_count()[mask]))
                        .filter(|(_, candidates_count)| *candidates_count == 1)
                        .map(|(index, _)| index)
                        .collect();

                    if !single_candidate_indices.is_empty() {
                        let pick_single_candidate_index =
                            rng.gen_range(0..single_candidate_indices.len()); // .Next(singleCandidateIndices.Length);
                        let single_candidate_index = single_candidate_indices[pick_single_candidate_index];
                        let candidate_mask = candidate_masks[single_candidate_index as usize];
                        let candidate: i32 = lookups.single_bit_to_index()[&candidate_mask];

                        let row = single_candidate_index / 9;
                        let col = single_candidate_index % 9;

                        state[single_candidate_index as usize] = (candidate + 1) as usize;

                        candidate_masks[single_candidate_index as usize] = 0;
                        change_made = true;

                        println!(
                            "({}, {}) can only contain {}.",
                            row + 1,
                            col + 1,
                            candidate + 1
                        );
                    }

                    // #endregion

                    // #region Try to find a number which can only appear in one place in a row/column/block

                    if !change_made {
                        let mut group_descriptions = List::new(); // new List<string>();
                        let mut candidate_row_indices = List::new(); //  new List<int>();
                        let mut candidate_col_indices = List::new(); // new List<int>();
                        let mut candidates = List::new(); // new List<int>();

                        for digit in 1..=9 {
                            let mask = 1 << (digit - 1);
                            for cell_group in 0..9 {
                                let mut row_number_count = 0;
                                let mut index_in_row = 0;

                                let mut col_number_count = 0;
                                let mut index_in_col = 0;

                                let mut block_number_count = 0;
                                let mut index_in_block = 0;

                                for index_in_group in 0..9 {
                                    let row_state_index = 9 * cell_group + index_in_group;
                                    let col_state_index = 9 * index_in_group + cell_group;
                                    let block_row_index = (cell_group / 3) * 3 + index_in_group / 3;
                                    let block_col_index = (cell_group % 3) * 3 + index_in_group % 3;
                                    let block_state_index = block_row_index * 9 + block_col_index;

                                    if (candidate_masks[row_state_index] & mask) != 0 {
                                        row_number_count += 1;
                                        index_in_row = index_in_group;
                                    }

                                    if (candidate_masks[col_state_index] & mask) != 0 {
                                        col_number_count += 1;
                                        index_in_col = index_in_group;
                                    }

                                    if (candidate_masks[block_state_index] & mask) != 0 {
                                        block_number_count += 1;
                                        index_in_block = index_in_group;
                                    }
                                }

                                if row_number_count == 1 {
                                    group_descriptions.add(format!("Row #{}", cell_group + 1));
                                    candidate_row_indices.add(cell_group);
                                    candidate_col_indices.add(index_in_row);
                                    candidates.add(digit);
                                }

                                if col_number_count == 1 {
                                    group_descriptions.add(format!("Column #{}", cell_group + 1));
                                    candidate_row_indices.add(index_in_col);
                                    candidate_col_indices.add(cell_group);
                                    candidates.add(digit);
                                }

                                if block_number_count == 1 {
                                    let block_row = cell_group / 3;
                                    let block_col = cell_group % 3;

                                    group_descriptions.add(format!(
                                        "Block ({}, {})",
                                        block_row + 1,
                                        block_col + 1
                                    ));
                                    candidate_row_indices.add(block_row * 3 + index_in_block / 3);
                                    candidate_col_indices.add(block_col * 3 + index_in_block % 3);
                                    candidates.add(digit);
                                }
                            }
                        }

                        if !candidates.is_empty() {
                            let index = rng.gen_range(0..candidates.len());
                            let description = group_descriptions.at(index);
                            let row = candidate_row_indices.at(index);
                            let col = candidate_col_indices.at(index);
                            let digit = candidates.at(index);

                            let message = format!(
                                "{} can contain {} only at ({}, {}).",
                                description,
                                digit,
                                row + 1,
                                col + 1
                            );

                            let state_index = 9 * row + col;
                            state[state_index] = *digit;
                            candidate_masks[state_index] = 0;

                            change_made = true;

                            println!("{}", message);
                        }
                    }

                    // #endregion

                    // #region Try to find pairs of digits in the same row/column/block and remove them from other colliding cells
                    if !change_made {
                        let two_digit_masks = candidate_masks
                            .iter()
                            .filter(|mask| lookups.mask_to_ones_count()[mask] == 2)
                            .cloned()
                            .collect::<BTreeSet<i32>>();

                        let groups = {
                            let two_digit_iter = two_digit_masks.iter();
                            two_digit_iter
                                .map(|mask| {
                                    cell_groups
                                        .iter()
                                        .filter(|group| {
                                            group.count(|tuple| {
                                                candidate_masks[tuple.index as usize] == *mask
                                            }) == 2
                                        })
                                        .filter(|group| {
                                            group.any(|tuple| {
                                                candidate_masks[tuple.index as usize] != *mask
                                                    && (candidate_masks[tuple.index as usize] & mask)
                                                    > 0
                                            })
                                        })
                                        .map(|group| TwoDigitMask {
                                            mask: *mask,
                                            description: group.first().unwrap().description.clone(),
                                            cells: group.clone(),
                                        })
                                        .collect::<Vec<_>>()
                                })
                                .flatten()
                                .collect::<Vec<_>>()
                        };

                        for group in &groups {
                            let cells = group
                                .cells
                                .data
                                .iter()
                                .filter(|cell| {
                                    candidate_masks[cell.index as usize] != group.mask
                                        && (candidate_masks[cell.index as usize] & group.mask)
                                        > 0
                                })
                                .collect::<Vec<_>>();

                            let mask_cells = group
                                .cells
                                .data
                                .iter()
                                .filter(|cell| {
                                    candidate_masks[cell.index as usize] == group.mask
                                })
                                .collect::<Vec<_>>();

                            if !cells.is_empty() {
                                let mut upper = 0;
                                let mut lower = 0;
                                let mut temp = group.mask;

                                let mut value = 1;
                                while temp > 0 {
                                    if (temp & 1) > 0 {
                                        lower = upper;
                                        upper = value;
                                    }
                                    temp >>= 1;
                                    value += 1;
                                }

                                println!(
                                    "Values {} and {} in {} are in cells ({}, {}) and ({}, {}).",
                                    lower,
                                    upper,
                                    group.description,
                                    mask_cells[0].row + 1,
                                    mask_cells[0].col + 1,
                                    mask_cells[1].row + 1,
                                    mask_cells[1].col + 1
                                );

                                for cell in cells {
                                    let mut mask_to_remove =
                                        candidate_masks[cell.index as usize] & group.mask;
                                    let mut values_to_remove = List::new(); // new List<int>();
                                    let mut cur_value = 1;
                                    while mask_to_remove > 0 {
                                        if (mask_to_remove & 1) > 0 {
                                            values_to_remove.add(cur_value);
                                        }
                                        mask_to_remove >>= 1;
                                        cur_value += 1;
                                    }

                                    let values_report = values_to_remove
                                        .data
                                        .iter()
                                        .map(|i| i.to_string())
                                        .collect::<Vec<_>>();

                                    let values_report = values_report.join("\n");

                                    println!(
                                        "{} cannot appear in ({}, {}).",
                                        values_report,
                                        cell.row + 1,
                                        cell.col + 1
                                    );

                                    candidate_masks[cell.index as usize] &= !group.mask;
                                    step_change_made = true;
                                }
                            }
                        }
                    }
                    // #endregion

                    // #region Try to find groups of digits of size N which only appear in N cells within row/column/block
                    // When a set of N digits only appears in N cells within row/column/block, then no other digit can appear in the same set of cells
                    // All other candidates can then be removed from those cells

                    if !change_made && !step_change_made {
                        let masks = lookups.mask_to_ones_count()
                            .iter()
                            .filter(|(_, value)| **value > 1)
                            .map(|(key, _)| key)
                            .cloned()
                            .collect::<Vec<_>>();

                        let groups_with_n_masks = {
                            masks
                                .iter()
                                .map(|mask| {
                                    let groups_iter = cell_groups.iter();
                                    let filtered_groups: Vec<_> = groups_iter
                                        .filter(|group| {
                                            group.data.iter().all(|cell| {
                                                state[cell.index as usize] == 0
                                                    || (mask
                                                    & (1 << (state[cell.index as usize] - 1)))
                                                    == 0
                                            })
                                        })
                                        .collect();

                                    filtered_groups
                                        .iter()
                                        .map(|group| -> NMaskGroups {
                                            NMaskGroups {
                                                mask: *mask,
                                                description: group
                                                    .data
                                                    .first()
                                                    .unwrap()
                                                    .description
                                                    .clone(),
                                                cells: (*group).clone(),
                                                cells_with_masks: group
                                                    .data
                                                    .iter()
                                                    .filter(|cell| {
                                                        state[cell.index as usize] == 0
                                                            && (candidate_masks[cell.index as usize]
                                                            & mask)
                                                            != 0
                                                    })
                                                    .cloned()
                                                    .collect::<Vec<_>>(),
                                            }
                                        })
                                        .collect::<Vec<_>>()
                                })
                                .flatten()
                                .filter(|group| {
                                    group.cells_with_masks.len()
                                        == lookups.mask_to_ones_count()[&group.mask()] as usize
                                })
                                .collect::<Vec<_>>()
                        };

                        for group_with_n_masks in groups_with_n_masks {
                            let mask = group_with_n_masks.mask;

                            let cond = group_with_n_masks.cells.any(|cell| {
                                (candidate_masks[cell.index as usize] & mask) != 0
                                    && (candidate_masks[cell.index as usize] & !mask) != 0
                            });
                            if cond {
                                let mut message = String::new();
                                message += format!("In {} values ", &group_with_n_masks.description).as_str();

                                Program::append_mask_to_message(&mut message, mask);

                                message.push_str(" appear only in cells");
                                for cell in &group_with_n_masks.cells_with_masks {
                                    message += format!(" ({}, {})", cell.row + 1, cell.col + 1).as_str();
                                }

                                message.push_str(" and other values cannot appear in those cells.");

                                println!("{}", message);
                            }

                            for cell in group_with_n_masks.cells_with_masks {
                                let mask_to_clear =
                                    candidate_masks[cell.index as usize] & !group_with_n_masks.mask;
                                if mask_to_clear == 0 {
                                    continue;
                                }

                                candidate_masks[cell.index as usize] &= group_with_n_masks.mask;
                                step_change_made = true;


                                let mut message = String::new();

                                Program::append_mask_to_message(&mut message, mask_to_clear);

                                // message += format!(" cannot appear in cell ({}, {}).",
                                //                    cell.3 + 1,
                                //                    cell.4 + 1).as_str();
                                println!("{} cannot appear in cell ({}, {}).", message,
                                         cell.row + 1,
                                         cell.col + 1);
                            }
                        }
                    }

                    // #endregion
                }

                // #region Final attempt - look if the board has multiple solutions
                if !change_made {
                    // This is the last chance to do something in this iteration:
                    // If this attempt fails, board will not be entirely solved.

                    // Try to see if there are pairs of values that can be exchanged arbitrarily
                    // This happens when board has more than one valid solution

                    let mut candidate_index1 = VecDeque::new(); // new Queue<int>();
                    let mut candidate_index2 = VecDeque::new(); // new Queue<int>();
                    let mut candidate_digit1 = VecDeque::new(); // new Queue<int>();
                    let mut candidate_digit2 = VecDeque::new(); // new Queue<int>();

                    for i in 0..(candidate_masks.len() - 1) {
                        if lookups.mask_to_ones_count()[&candidate_masks[i]] == 2 {
                            let row = i / 9;
                            let col = i % 9;
                            let block_index = 3 * (row / 3) + col / 3;

                            let mut temp = candidate_masks[i];
                            let mut lower = 0;
                            let mut upper = 0;
                            let mut digit = 1;
                            while temp > 0 {
                                if (temp & 1) != 0 {
                                    lower = upper;
                                    upper = digit;
                                }
                                temp >>= 1;
                                digit += 1;
                            }

                            for j in (i + 1)..(candidate_masks.len()) {
                                if candidate_masks[j] == candidate_masks[i] {
                                    let row1 = j / 9;
                                    let col1 = j % 9;
                                    let block_index1 = 3 * (row1 / 3) + col1 / 3;

                                    if row == row1 || col == col1 || block_index == block_index1 {
                                        candidate_index1.push_back(i);
                                        candidate_index2.push_back(j);
                                        candidate_digit1.push_back(lower);
                                        candidate_digit2.push_back(upper);
                                    }
                                }
                            }
                        }
                    }

                    // At this point we have the lists with pairs of cells that might pick one of two digits each
                    // Now we have to check whether that is really true - does the board have two solutions?

                    let mut state_index1 = List::new();
                    let mut state_index2 = List::new();
                    let mut value1 = List::new();
                    let mut value2 = List::new();

                    while !candidate_index1.is_empty() {
                        let index1 = candidate_index1.pop_front().unwrap();
                        let index2 = candidate_index2.pop_front().unwrap();
                        let digit1 = candidate_digit1.pop_front().unwrap();
                        let digit2 = candidate_digit2.pop_front().unwrap();

                        let mut alternate_state = state.clone();

                        if final_state[index1] == digit1 {
                            alternate_state[index1] = digit2;
                            alternate_state[index2] = digit1;
                        } else {
                            alternate_state[index1] = digit1;
                            alternate_state[index2] = digit2;
                        }

                        let alternate_state = alternate_state;

                        // What follows below is a complete copy-paste of the solver which appears at the beginning of this method
                        // However, the algorithm couldn't be applied directly and it had to be modified.
                        // Implementation below assumes that the board might not have a solution.

                        let mut stacks = Stacks::new();

                        command = Commands::Expand;
                        loop {
                            command = match command {
                                Commands::Expand => stacks.do_expand(&mut rng, &alternate_state),
                                Commands::Collapse => {
                                    let command = stacks.do_collapse();

                                    if stacks.is_empty() {
                                        Commands::Fail
                                    } else {
                                        command
                                    }
                                }
                                Commands::Move => stacks.do_move_2(),
                                Commands::Complete => break,
                                Commands::Fail => break,
                            };
                        }

                        if command == Commands::Complete {
                            // Board was solved successfully even with two digits swapped
                            state_index1.add(index1);
                            state_index2.add(index2);
                            value1.add(digit1);
                            value2.add(digit2);
                        }
                    }

                    if !state_index1.data.is_empty() {
                        let pos = rng.gen_range(0..state_index1.data.len());
                        let index1 = state_index1.at(pos);
                        let index2 = state_index2.at(pos);
                        let digit1 = value1.at(pos);
                        let digit2 = value2.at(pos);
                        let row1 = index1 / 9;
                        let col1 = index1 % 9;
                        let row2 = index2 / 9;
                        let col2 = index2 % 9;

                        let description;

                        if index1 / 9 == index2 / 9 {
                            description = format!("row #{}", index1 / 9 + 1);
                        } else if index1 % 9 == index2 % 9 {
                            description = format!("column #{}", index1 % 9 + 1);
                        } else {
                            description = format!("block ({}, {})", row1 / 3 + 1, col1 / 3 + 1);
                        }

                        state[*index1] = final_state[*index1];
                        state[*index2] = final_state[*index2];
                        candidate_masks[*index1] = 0;
                        candidate_masks[*index2] = 0;
                        change_made = true;


                        println!("Guessing that {} and {} are arbitrary in {} (multiple solutions): Pick {}->({}, {}), {}->({}, {}).",
                                 digit1, digit2, description,
                                 final_state[*index1], row1 + 1, col1 + 1,
                                 final_state[*index2], row2 + 1, col2 + 1
                        );
                    }
                }
                // #endregion

                if change_made {
                    // #region Print the board as it looks after one change was made to it
                    println!("{}", Board::state_to_printable_board(&state));
                    let code: String = state.code();

                    println!("Code: {}", code);

                    println!("{}", "");
                    // #endregion
                }
            }
        }


        fn append_mask_to_message(message: &mut String, mut mask_to_clear: i32) {
            {
                let mut value_to_clear = 1;
                let mut separator = "";

                while mask_to_clear > 0 {
                    if (mask_to_clear & 1) > 0 {
                        *message += format!("{}{}", separator, value_to_clear).as_str();
                        separator = ", ";
                    }
                    mask_to_clear >>= 1;
                    value_to_clear += 1;
                }
            }
        }
    }
}

fn main() {
    sudoku_kata::Program::play();
}