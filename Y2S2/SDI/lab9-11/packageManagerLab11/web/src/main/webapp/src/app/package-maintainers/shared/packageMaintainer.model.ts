export class PackageMaintainerModel {
  id: number;
  maintainerID: number;
  packageID: number;
  packageName: string;
  importance: number;
}

export const emptyPackageMaintainer: PackageMaintainerModel = {
  id: 0,
  maintainerID: 0,
  packageID: 0,
  packageName: "",
  importance: 0
}
