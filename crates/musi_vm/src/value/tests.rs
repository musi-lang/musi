    use super::*;

    #[test]
    fn int_clone_is_independent() {
        let a = Value::Int(42);
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn string_clone_shares_allocation() {
        let a = Value::String(Rc::from("hello"));
        let b = a.clone();
        assert_eq!(a, b);
        // Both must be String variants pointing to the same allocation.
        assert!(matches!(&a, Value::String(_)));
        if let (Value::String(ra), Value::String(rb)) = (&a, &b) {
            assert!(Rc::ptr_eq(ra, rb));
        }
    }

    #[test]
    fn display_formats() {
        assert_eq!(Value::Int(-7).to_string(), "-7");
        assert_eq!(Value::Bool(true).to_string(), "true");
        assert_eq!(Value::String(Rc::from("hi")).to_string(), "hi");
        assert_eq!(Value::Unit.to_string(), "()");
    }
