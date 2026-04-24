#![allow(unused_imports)]

use crate::{
    HilBinaryOp, HilBlock, HilFunction, HilInstruction, HilModule, HilParam, HilShape,
    HilTerminator, HilType, HilValueId, HilVerifyError, format_hil, parse_hil,
};

fn int_ty() -> HilType {
    HilType::new("Int")
}

fn bool_ty() -> HilType {
    HilType::new("Bool")
}

fn sample_module() -> HilModule {
    let int = int_ty();
    HilModule::new(
        "main",
        [HilFunction::new(
            "addOne",
            [HilParam::new("n", HilValueId(0), int.clone())],
            Some(int.clone()),
            [HilBlock::new(
                "entry",
                [
                    HilInstruction::ConstInt {
                        out: HilValueId(1),
                        ty: int.clone(),
                        value: 1,
                    },
                    HilInstruction::Binary {
                        out: HilValueId(2),
                        op: HilBinaryOp::IntAdd,
                        ty: int,
                        left: HilValueId(0),
                        right: HilValueId(1),
                    },
                ],
                HilTerminator::Return(Some(HilValueId(2))),
            )],
        )],
    )
}

mod success {
    use super::*;

    #[test]
    fn verifies_valid_hil_module() {
        sample_module().verify().expect("HIL verifies");
    }

    #[test]
    fn formats_high_level_hil() {
        let text = format_hil(&sample_module());

        assert!(text.contains("module main"));
        assert!(text.contains("fn addOne(%0 n: Int) -> Int"));
        assert!(text.contains("%2: Int = int.add %0, %1"));
    }

    #[test]
    fn roundtrips_hil_text() {
        let module = sample_module();
        let text = format_hil(&module);
        let parsed = parse_hil(&text).expect("HIL parses");
        assert_eq!(parsed, module);
    }

    #[test]
    fn accepts_declared_effect_capability() {
        let int = int_ty();
        let module = HilModule::new(
            "main",
            [HilFunction::new(
                "read",
                [],
                Some(int.clone()),
                [HilBlock::new(
                    "entry",
                    [HilInstruction::EffectCall {
                        out: HilValueId(0),
                        result_ty: int,
                        effect: "Console".into(),
                        op: "readLine".into(),
                        args: Box::new([]),
                    }],
                    HilTerminator::Return(Some(HilValueId(0))),
                )],
            )
            .with_capabilities([HilShape::Effect])],
        );

        module.verify().expect("effect capability declared");
    }
}

mod failure {
    use super::*;

    #[test]
    fn rejects_use_before_definition() {
        let int = int_ty();
        let module = HilModule::new(
            "main",
            [HilFunction::new(
                "bad",
                [],
                Some(int.clone()),
                [HilBlock::new(
                    "entry",
                    [HilInstruction::Binary {
                        out: HilValueId(2),
                        op: HilBinaryOp::IntAdd,
                        ty: int,
                        left: HilValueId(0),
                        right: HilValueId(1),
                    }],
                    HilTerminator::Return(Some(HilValueId(2))),
                )],
            )],
        );

        assert!(matches!(
            module.verify(),
            Err(HilVerifyError::UndefinedValue {
                value: HilValueId(0)
            })
        ));
    }

    #[test]
    fn rejects_type_mismatch() {
        let int = int_ty();
        let module = HilModule::new(
            "main",
            [HilFunction::new(
                "bad",
                [HilParam::new("flag", HilValueId(0), bool_ty())],
                Some(int.clone()),
                [HilBlock::new(
                    "entry",
                    [HilInstruction::ConstInt {
                        out: HilValueId(1),
                        ty: int,
                        value: 1,
                    }],
                    HilTerminator::Return(Some(HilValueId(0))),
                )],
            )],
        );

        assert!(matches!(
            module.verify(),
            Err(HilVerifyError::ReturnTypeMismatch { .. })
        ));
    }

    #[test]
    fn rejects_missing_effect_capability() {
        let int = int_ty();
        let module = HilModule::new(
            "main",
            [HilFunction::new(
                "bad",
                [],
                Some(int.clone()),
                [HilBlock::new(
                    "entry",
                    [HilInstruction::EffectCall {
                        out: HilValueId(0),
                        result_ty: int,
                        effect: "Console".into(),
                        op: "readLine".into(),
                        args: Box::new([]),
                    }],
                    HilTerminator::Return(Some(HilValueId(0))),
                )],
            )],
        );

        assert!(matches!(
            module.verify(),
            Err(HilVerifyError::ShapeRequired {
                capability: HilShape::Effect
            })
        ));
    }
}
