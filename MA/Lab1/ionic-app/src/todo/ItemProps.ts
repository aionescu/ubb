export interface ItemData {
  num: number
  str: string
  date: Date
  bool: boolean
}

export interface ItemProps {
  id?: string
  data: ItemData
}
