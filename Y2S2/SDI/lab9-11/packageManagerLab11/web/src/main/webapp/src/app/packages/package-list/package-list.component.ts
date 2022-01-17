import { Component, OnInit } from '@angular/core';
import { PackageModel } from '../shared/package.model';
import { PackageService } from '../shared/package.service';

@Component({
  selector: 'app-package-list',
  templateUrl: './package-list.component.html',
  styleUrls: ['./package-list.component.css']
})
export class PackageListComponent implements OnInit {
  packages: PackageModel[];

  constructor(private packageService: PackageService) { }

  ngOnInit(): void {
    this.packageService.getPackages().subscribe(packages => this.packages = packages)
  }

  filter(field: keyof PackageModel, searchTerm: string) {
    this.packages = this.packages.filter(pkg => String(pkg[field]).includes(searchTerm))
  }

  sort(field: keyof PackageModel, order: "ascending" | "descending") {
    const cmp = (a: PackageModel, b: PackageModel) =>
    a[field] < b[field] ? -1
    : a[field] > b[field] ? 1
    : 0

    const orderMultiplier = order === "ascending" ? 1 : -1
    const cmpOrdered = (a: PackageModel, b: PackageModel) => cmp(a, b) * orderMultiplier

    this.packages.sort(cmpOrdered)
  }
}
