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
}
