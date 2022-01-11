import dataStore from 'nedb-promise';

export class SpecialEventStore {
  constructor({ filename, autoload }) {
    this.store = dataStore({ filename, autoload });
  }
  
  async find(props) {
    return this.store.find(props);
  }
  
  async findOne(props) {
    return this.store.findOne(props);
  }
  
  async insert(specialEvent) {
    let specialEventTitle = specialEvent.title;
    if (!specialEventTitle) {
      throw new Error('Missing title property')
    }
    let specialEventNumberOfPeople = specialEvent.numberOfPeople;
    if (specialEventNumberOfPeople === undefined || specialEventNumberOfPeople === null) {
      throw new Error('Missing number of people property')
    }
    let specialEventDate = specialEvent.date;
    if (specialEventDate === undefined || specialEventDate === null) {
      throw new Error('Missing date property')
    }
    let specialEventIsApproved = specialEvent.isApproved;
    if (specialEventIsApproved === undefined || specialEventIsApproved === null) {
      throw new Error('Missing is approved property')
    }
    return this.store.insert(specialEvent);
  };
  
  async update(props, specialEvent) {
    return this.store.update(props, specialEvent);
  }
  
  async remove(props) {
    return this.store.remove(props);
  }
}

export default new SpecialEventStore({ filename: './db/specialEvents.json', autoload: true });