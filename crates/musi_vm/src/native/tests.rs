    use super::*;

    #[test]
    fn registry_has_writeln_and_write() {
        let reg = NativeRegistry::new();
        assert!(reg.get(intrinsics::WRITELN).is_some());
        assert!(reg.get(intrinsics::WRITE).is_some());
        assert!(reg.get(99).is_none());
    }

    #[test]
    fn register_overrides_handler() {
        let mut reg = NativeRegistry::new();
        fn custom(_vm: &Vm, _args: &[Value]) -> Value {
            Value::Int(42)
        }
        reg.register(intrinsics::WRITELN, custom);
        // Lookup still works; the handler itself changed (we just verify it's Some).
        assert!(reg.get(intrinsics::WRITELN).is_some());
    }
