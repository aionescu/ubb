export class MaintainerModel {
  id: number;
  userName: string;
  fullName: string;
  email: string;
}

export const emptyMaintainer: MaintainerModel = {
  id: 0,
  userName: "",
  fullName: "",
  email: ""
}
