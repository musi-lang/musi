export interface MsTaskDefinitionObject {
	command: string;
	description?: string;
	dependencies?: string[];
}

export type MsTaskDefinition = string | MsTaskDefinitionObject;

export interface MsWorkspaceDefinition {
	members?: string[];
}

export interface MsPackageManifest {
	name?: string;
	version?: string;
	entry?: string;
	main?: string;
	imports?: Record<string, string>;
	workspace?: MsWorkspaceDefinition | string[];
	tasks?: Record<string, MsTaskDefinition>;
}

export interface MsTaskSpec {
	name: string;
	command: string;
	description?: string;
	dependencies: string[];
}

export interface PackageRoot {
	manifestUri: string;
	manifestPath: string;
	rootDir: string;
	mainEntry: string;
	manifest: MsPackageManifest;
}
