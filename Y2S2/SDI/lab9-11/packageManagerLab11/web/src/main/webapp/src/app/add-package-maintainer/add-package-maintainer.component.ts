import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { PackageMaintainerModel, emptyPackageMaintainer } from "../package-maintainers/shared/packageMaintainer.model";
import { PackageMaintainerService } from "../package-maintainers/shared/packageMaintainer.service";

@Component({
	selector: "app-add-package-maintainer",
	templateUrl: "./add-package-maintainer.component.html",
	styleUrls: ["./add-package-maintainer.component.css"],
	providers: [PackageMaintainerService]
})
export class AddPackageMaintainerComponent implements OnInit {
	packageMaintainer: PackageMaintainerModel = emptyPackageMaintainer;

	constructor(private router: Router, private service: PackageMaintainerService) { }

	ngOnInit(): void { }

	async onAdd() {
		this.service.addPackageMaintainer(this.packageMaintainer).subscribe(() => this.router.navigate(["../package-maintainers"]));
	}
}
