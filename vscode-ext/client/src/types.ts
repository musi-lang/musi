export interface MsPackageTask {
	command: string;
	description?: string;
}

export interface MsPackage {
	tasks?: Record<string, string | MsPackageTask>;
}
