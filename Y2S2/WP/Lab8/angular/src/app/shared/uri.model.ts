export interface URI {
  id: number;
  uri: string;
  description: string;
  category: string;
  owner: number;
}

export const emptyURI: URI = {
  id: 0,
  uri: "",
  description: "",
  category: "",
  owner: 0
}
