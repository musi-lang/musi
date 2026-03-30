use music_basic::SourceMap;
use music_check::AnalyzedModule;
use music_names::Interner;

#[derive(Debug)]
pub struct EmitModule<'a> {
    pub path: &'a str,
    pub analyzed: &'a AnalyzedModule,
}

#[derive(Debug)]
pub struct EmitProgram<'a> {
    pub interner: &'a Interner,
    pub sources: &'a SourceMap,
    pub modules_in_order: Box<[EmitModule<'a>]>,
    pub entry_path: &'a str,
}

#[derive(Debug)]
pub struct ProgramArtifact {
    pub artifact: music_il::SeamArtifact,
}
