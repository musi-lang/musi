#[derive(Clone)]
pub struct EffectHandler {
    pub effect_id: u16,
    pub op_id: u16,
    pub handler_frame_depth: usize,
    pub handler_pc: usize,
    pub saved_stack_depth: usize,
}
