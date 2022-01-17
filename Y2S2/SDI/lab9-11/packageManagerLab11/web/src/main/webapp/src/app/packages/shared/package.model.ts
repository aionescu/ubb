export class PackageModel {
  id: number;
  name: string;
  description: string;
  sourceRepo: string;
  license: string;
}

export const emptyPackage: PackageModel = {
  id: 0,
  name: "",
  description: "",
  sourceRepo: "",
  license: ""
}
