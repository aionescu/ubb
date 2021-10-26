export interface ItemData {
  num: number
  str: string
  date: Date
  bool: boolean
}

export const formatDate = (date: Date) => date.toLocaleDateString("en-CA")

export interface ItemProps {
  id?: string
  data: ItemData
}

export const deserializeItem = (item: ItemProps) =>
  ({ ...item, data: { ...item.data, date: new Date(item.data.date) }})
