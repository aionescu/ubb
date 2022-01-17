import { Component, OnInit } from '@angular/core';
import { PackageVersionModel } from '../shared/packageVersion.model';
import { PackageVersionService } from '../shared/packageVersion.service';

@Component({
  selector: 'app-package-version-list',
  templateUrl: './package-version-list.component.html',
  styleUrls: ['./package-version-list.component.css']
})
export class PackageVersionListComponent implements OnInit {
  packageVersions: PackageVersionModel[];

  constructor(private packageService: PackageVersionService) { }

  ngOnInit(): void { }

  loadPackageVersions(packageID: number) {
    this.packageService.getPackageVersions(packageID).subscribe(packageVersions => this.packageVersions = packageVersions)
  }

  filter(field: keyof PackageVersionModel, searchTerm: string) {
    this.packageVersions = this.packageVersions.filter(pkg => String(pkg[field]).includes(searchTerm))
  }

  sort(field: keyof PackageVersionModel, order: "ascending" | "descending") {
    const cmp = (a: PackageVersionModel, b: PackageVersionModel) =>
    a[field] < b[field] ? -1
    : a[field] > b[field] ? 1
    : 0

    const orderMultiplier = order === "ascending" ? 1 : -1
    const cmpOrdered = (a: PackageVersionModel, b: PackageVersionModel) => cmp(a, b) * orderMultiplier

    this.packageVersions.sort(cmpOrdered)
  }
}
