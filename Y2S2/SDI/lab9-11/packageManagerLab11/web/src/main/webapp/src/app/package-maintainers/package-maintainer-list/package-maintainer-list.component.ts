import { Component, OnInit } from '@angular/core';
import { PackageMaintainerModel } from '../shared/packageMaintainer.model';
import { PackageMaintainerService } from '../shared/packageMaintainer.service';

@Component({
  selector: 'app-package-maintainer-list',
  templateUrl: './package-maintainer-list.component.html',
  styleUrls: ['./package-maintainer-list.component.css']
})
export class PackageMaintainerListComponent implements OnInit {
  packageMaintainers: PackageMaintainerModel[];

  constructor(private packageService: PackageMaintainerService) { }

  ngOnInit(): void { }

  loadPackageMaintainers(maintainerID: number) {
    this.packageService.getPackageMaintainers(maintainerID).subscribe(packageMaintainers => this.packageMaintainers = packageMaintainers)
  }

  filter(field: keyof PackageMaintainerModel, searchTerm: string) {
    this.packageMaintainers = this.packageMaintainers.filter(pkg => String(pkg[field]).includes(searchTerm))
  }

  sort(field: keyof PackageMaintainerModel, order: "ascending" | "descending") {
    const cmp = (a: PackageMaintainerModel, b: PackageMaintainerModel) =>
    a[field] < b[field] ? -1
    : a[field] > b[field] ? 1
    : 0

    const orderMultiplier = order === "ascending" ? 1 : -1
    const cmpOrdered = (a: PackageMaintainerModel, b: PackageMaintainerModel) => cmp(a, b) * orderMultiplier

    this.packageMaintainers.sort(cmpOrdered)
  }
}
