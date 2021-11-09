import dataStore from 'nedb-promise';

export class PackageStore {
  constructor({ filename, autoload }) {
    this.store = dataStore({ filename, autoload });
  }

  async find(props) {
    return this.store.find(props);
  }

  async findOne(props) {
    return this.store.findOne(props);
  }

  async insert(pkg) {
    return this.store.insert(pkg);
  };

  async update(props, pkg) {
    return this.store.update(props, pkg);
  }

  async remove(props) {
    return this.store.remove(props);
  }
}

export default new PackageStore({ filename: './db/packages.json', autoload: true });
