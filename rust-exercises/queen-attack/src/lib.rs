#[derive(Debug)]
pub struct ChessPosition(i32, i32);

#[derive(Debug)]
pub struct Queen {
    position: ChessPosition
}

impl ChessPosition {
    pub fn new(rank: i32, file: i32) -> Option<Self> {
        if (0 <= rank && rank < 8) && (0 <= file && file < 8){
            Some(ChessPosition(rank, file))
        } else {
            None
        }
    }
}

impl Queen {
    pub fn new(position: ChessPosition) -> Self {
        Self { position }
    }

    pub fn can_attack(&self, other: &Queen) -> bool {
        return if self.is_on_same_tile(other) {
            false
        } else {
            self.can_straight_attack(other) || self.can_diagonal_attack(other)
        }
    }

    fn is_on_same_tile(&self, other: &Queen) -> bool {
        return if (self.position.1 == other.position.1) && (self.position.0 == other.position.0) {
            true
        } else {
            false
        }
    }

    fn can_straight_attack(&self, other: &Queen) -> bool {
        return if (self.position.0 == other.position.0) || (self.position.1 == other.position.1) {
            true
        } else {
            false
        }
    }

    fn can_diagonal_attack(&self, other: &Queen) -> bool {
        return if (self.position.0 - other.position.0).abs() == (self.position.1 - other.position.1).abs() {
            true
        } else {
            false
        }
    }
}
