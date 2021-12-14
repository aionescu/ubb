export interface ItemData {
  packageName: string
  latestVersion: number
  uploadDate: Date
  isDeprecated: boolean
  latitude: number
  longitude: number
}

export const defaultItemData: ItemData = {
  packageName: "",
  latestVersion: 0,
  uploadDate: new Date(Date.now()),
  isDeprecated: false,
  latitude: 0,
  longitude: 0
}

export const formatDate = (date: Date) => date.toLocaleDateString("en-CA")

export interface ItemProps {
  _id?: string
  data: ItemData
}

export const deserializeItem = (item: ItemProps) =>
  ({ ...item, data: { ...item.data, uploadDate: new Date(item.data.uploadDate) }})

export const filterItems = (
  items: ItemProps[],
  deprecatedFilter: boolean,
  nameFilter?: string,
  versionFilter?: string,
  dateFilter?: string) =>
{
  const filterFn = (item: ItemProps) =>
    (!nameFilter || item.data.packageName.includes(nameFilter)) &&
    (!versionFilter || item.data.latestVersion.toString().includes(versionFilter)) &&
    (!dateFilter || formatDate(item.data.uploadDate).includes(dateFilter)) &&
    (!deprecatedFilter || item.data.isDeprecated === deprecatedFilter)

  return items.filter(filterFn)
}
