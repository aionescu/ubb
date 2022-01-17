import { Component, OnInit } from '@angular/core';
import { MaintainerModel } from '../shared/maintainer.model';
import { MaintainerService } from '../shared/maintainer.service';

@Component({
  selector: 'app-maintainer-list',
  templateUrl: './maintainer-list.component.html',
  styleUrls: ['./maintainer-list.component.css']
})
export class MaintainerListComponent implements OnInit {
  maintainers: MaintainerModel[];

  constructor(private maintainerService: MaintainerService) { }

  ngOnInit(): void {
    this.maintainerService.getMaintainers().subscribe(maintainers => this.maintainers = maintainers)
  }

  filter(field: keyof MaintainerModel, searchTerm: string) {
    this.maintainers = this.maintainers.filter(pkg => String(pkg[field]).includes(searchTerm))
  }

  sort(field: keyof MaintainerModel, order: "ascending" | "descending") {
    const cmp = (a: MaintainerModel, b: MaintainerModel) =>
    a[field] < b[field] ? -1
    : a[field] > b[field] ? 1
    : 0

    const orderMultiplier = order === "ascending" ? 1 : -1
    const cmpOrdered = (a: MaintainerModel, b: MaintainerModel) => cmp(a, b) * orderMultiplier

    this.maintainers.sort(cmpOrdered)
  }
}
