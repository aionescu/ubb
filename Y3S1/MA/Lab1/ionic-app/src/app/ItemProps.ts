export interface ItemData {
  packageName: string
  latestVersion: number
  uploadDate: Date
  isDeprecated: boolean
}

export const defaultItemData: ItemData = {
  packageName: "",
  latestVersion: 0,
  uploadDate: new Date(Date.now()),
  isDeprecated: false
}

export const formatDate = (date: Date) => date.toLocaleDateString("en-CA")

export interface ItemProps {
  id?: string
  data: ItemData
}

export const deserializeItem = (item: ItemProps) =>
  ({ ...item, data: { ...item.data, uploadDate: new Date(item.data.uploadDate) }})
