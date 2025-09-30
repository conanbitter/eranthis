struct OccupationArea {
    time_start: u32,
    time_end: u32,
    mem_start: u32,
    mem_end: u32,
}

#[derive(Clone, Debug)]
struct AllocEntry {
    time_start: u32,
    time_end: u32,
    size: u32,
    offset: u32,
}

#[derive(Eq, Clone, Copy)]
struct Gap {
    offset: u32,
    size: u32,
}

pub struct FrameAllocator {
    allocs: Vec<AllocEntry>,
    step: u32,
    max_size: u32,
    current_size: u32,
    tries: u32,
    areas: Vec<OccupationArea>,
}

impl OccupationArea {
    fn intersect(&self, other: &OccupationArea) -> bool {
        other.time_start <= self.time_end
            && other.time_end >= self.time_start
            && other.mem_start <= self.mem_end
            && other.mem_end >= self.mem_start
    }
}

impl Gap {
    fn merge(&self, other: &Gap) -> Option<Gap> {
        if self.offset + self.size == other.offset {
            Some(Gap {
                offset: self.offset,
                size: self.size + other.size,
            })
        } else {
            None
        }
    }
}

impl PartialEq for Gap {
    fn eq(&self, other: &Self) -> bool {
        self.offset == other.offset
    }
}

impl PartialOrd for Gap {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Gap {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl FrameAllocator {
    pub fn new() -> FrameAllocator {
        FrameAllocator {
            allocs: vec![],
            step: 0,
            max_size: 0,
            current_size: 0,
            tries: 0,
            areas: vec![],
        }
    }

    pub fn reset(&mut self) {
        self.allocs.clear();
        self.step = 0;
        self.max_size = 0;
        self.current_size = 0;
    }

    pub fn alloc(&mut self, size: u32) -> usize {
        self.step += 1;
        self.allocs.push(AllocEntry {
            time_start: self.step,
            time_end: 0,
            size,
            offset: 0,
        });
        self.current_size += size;
        if self.max_size < self.current_size {
            self.max_size = self.current_size;
        }
        self.allocs.len() - 1
    }

    pub fn free(&mut self, id: usize) {
        self.step += 1;
        self.allocs[id].time_end = self.step;
        self.current_size -= self.allocs[id].size;
    }

    fn prepare_allocs(&mut self) {
        for entry in &mut self.allocs {
            if entry.time_end == 0 {
                entry.time_end = self.step + 1;
            }
        }
    }

    const MAX_TRIES: u32 = 2000000;

    fn calculate_optimal(&mut self, id: usize) -> bool {
        if id == self.allocs.len() {
            return true;
        }
        self.tries += 1;
        if self.tries > FrameAllocator::MAX_TRIES {
            return false;
        }

        let entry = self.allocs[id].clone();

        'offset_loop: for offset in 0..self.max_size - entry.size + 1 {
            let area = OccupationArea {
                time_start: entry.time_start,
                time_end: entry.time_end,
                mem_start: offset,
                mem_end: offset + entry.size - 1,
            };

            for other_area in &self.areas {
                if area.intersect(other_area) {
                    continue 'offset_loop;
                }
            }
            self.areas.push(area);
            if self.calculate_optimal(id + 1) {
                self.allocs[id].offset = offset;
                return true;
            }
            self.areas.pop();
        }
        false
    }

    fn calculate_greedy(&mut self) {
        let mut gaps: Vec<Gap> = vec![];
        let mut offsets: Vec<u32> = vec![0; self.allocs.len()];
        let mut current_size = 0;
        let mut last_time = 0;

        for (i, entry) in self.allocs.iter().enumerate() {
            let mut modified = false;
            for (j, tofree) in self.allocs.iter().enumerate() {
                if tofree.time_end > last_time && tofree.time_end <= entry.time_start {
                    if offsets[j] + tofree.size == current_size {
                        current_size -= tofree.size;
                    } else {
                        gaps.push(Gap {
                            offset: offsets[j],
                            size: tofree.size,
                        });
                        modified = true;
                    }
                }
            }

            if modified {
                gaps.sort();
                let mut merged: Vec<Gap> = vec![];
                let mut cur_gap = gaps[0];
                for gap in gaps.iter().skip(1) {
                    if let Some(new_gap) = cur_gap.merge(gap) {
                        cur_gap = new_gap;
                    } else {
                        merged.push(cur_gap);
                        cur_gap = *gap;
                    }
                }
                merged.push(cur_gap);
                gaps = merged;
            }

            let mut fitting_gap: Option<usize> = None;
            for (i, gap) in gaps.iter().enumerate() {
                if gap.size >= entry.size {
                    if let Some(best) = fitting_gap {
                        if gap.size < gaps[best].size {
                            fitting_gap = Some(i);
                        }
                    } else {
                        fitting_gap = Some(i);
                    }
                }
            }

            if let Some(best) = fitting_gap {
                offsets[i] = gaps[best].offset;
                if gaps[best].size < entry.size {
                    gaps[best].offset += entry.size;
                    gaps[best].size -= entry.size;
                } else {
                    gaps.swap_remove(best);
                }
            } else {
                offsets[i] = current_size;
                current_size += entry.size;
                if self.max_size < current_size {
                    self.max_size = current_size;
                }
            }
            last_time = entry.time_start;
        }

        for (i, offset) in offsets.iter().enumerate() {
            self.allocs[i].offset = *offset;
        }
    }

    pub fn calculate(&mut self) {
        self.prepare_allocs();
        self.tries = 0;
        self.areas.clear();
        if !self.calculate_optimal(0) {
            println!("fallback");
            self.calculate_greedy();
        }
        println!("Tries: {}", self.tries);
        //println!("{:?}", self.allocs);
    }

    pub fn get_offset(&self, id: usize) -> u32 {
        self.allocs[id].offset
    }

    pub fn get_frame_size(&self) -> u32 {
        self.max_size
    }
}
