import React from 'react';
import { IonButton, IonCheckbox, IonItem, IonLabel } from '@ionic/react';
import { ItemProps } from './ItemProps';

interface ItemPropsExt extends ItemProps {
  flip: (item: ItemProps) => Promise<void>;
}

const Item: React.FC<ItemPropsExt> = item => {
  const { id, text, status, version, flip } = item;

  return (
    <IonItem>
      <IonLabel>Task #{id}:</IonLabel>
      <IonLabel>{text}</IonLabel>
      <IonLabel>{version}</IonLabel>
      <IonButton onClick={async () => await flip(item)}>{status === "active" ? "Close" : "Reopen"}</IonButton>
    </IonItem>
  );
};

export default Item;
