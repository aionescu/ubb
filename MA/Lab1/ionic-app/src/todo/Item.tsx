import React from 'react';
import { IonCheckbox, IonItem, IonLabel } from '@ionic/react';
import { ItemProps } from './ItemProps';

interface ItemPropsExt extends ItemProps {
  onEdit: (id?: string) => void;
}

const Item: React.FC<ItemPropsExt> = ({ id, text, onEdit }) => {
  return (
    <IonItem onClick={() => onEdit(id)}>
      <IonLabel>Item #{id}:</IonLabel>
      <IonLabel>{text.num}</IonLabel>
      <IonLabel>{text.str}</IonLabel>
      <IonLabel>{text.date}</IonLabel>
      <IonCheckbox checked={text.bool}/>
    </IonItem>
  );
};

export default Item;
