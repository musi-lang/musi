// Tests for the parent module — extracted from the inline test block.
    use super::Span;

    #[test]
    fn end_is_start_plus_length() {
        let s = Span::new(10, 5);
        assert_eq!(s.end(), 15);
    }

    #[test]
    fn merge_adjacent() {
        let a = Span::new(0, 3);
        let b = Span::new(3, 4);
        let m = a.merge(b);
        assert_eq!(m.start, 0);
        assert_eq!(m.length, 7);
    }

    #[test]
    fn merge_overlapping() {
        let a = Span::new(2, 6); // 2..8
        let b = Span::new(5, 4); // 5..9
        let m = a.merge(b);
        assert_eq!(m.start, 2);
        assert_eq!(m.end(), 9);
    }

    #[test]
    fn merge_reversed_order_is_same() {
        let a = Span::new(10, 3);
        let b = Span::new(0, 5);
        assert_eq!(a.merge(b), b.merge(a));
    }

    #[test]
    fn dummy_is_zero_length_at_zero() {
        assert_eq!(Span::DUMMY.start, 0);
        assert_eq!(Span::DUMMY.length, 0);
        assert_eq!(Span::DUMMY.end(), 0);
    }
