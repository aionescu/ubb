export class PackageVersionModel {
  id: number;
  packageID: number;
  versionNumber: string;
  startingDate: string;
}

export const emptyPackageVersion: PackageVersionModel = {
  id: 0,
  packageID: 0,
  versionNumber: "",
  startingDate: ""
}
