import React from 'react';
import { IonCheckbox, IonItem, IonLabel } from '@ionic/react';
import { formatDate, ItemProps } from './ItemProps';

interface ItemPropsExt extends ItemProps {
  onEdit: (id?: string) => void;
}

const Item: React.FC<ItemPropsExt> = ({ id, data: { num, str, date, bool }, onEdit }) => {
  return (
    <IonItem onClick={() => onEdit(id)}>
      <IonLabel>Item #{id}:</IonLabel>
      <IonLabel>{num}</IonLabel>
      <IonLabel>{str}</IonLabel>
      <IonLabel>{formatDate(date)}</IonLabel>
      <IonCheckbox checked={bool}/>
    </IonItem>
  );
};

export default Item;
