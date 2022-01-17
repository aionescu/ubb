import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { PackageMaintainerModel, emptyPackageMaintainer } from "../package-maintainers/shared/packageMaintainer.model";
import { PackageMaintainerService } from "../package-maintainers/shared/packageMaintainer.service";

@Component({
	selector: "app-delete-package-maintainer",
	templateUrl: "./delete-package-maintainer.component.html",
	styleUrls: ["./delete-package-maintainer.component.css"],
	providers: [PackageMaintainerService]
})
export class DeletePackageMaintainerComponent implements OnInit {
	packageMaintainer: PackageMaintainerModel = emptyPackageMaintainer;

	constructor(private router: Router, private service: PackageMaintainerService) { }

	ngOnInit(): void { }

	async onDelete() {
		this.service.deletePackageMaintainer(this.packageMaintainer).subscribe(() => this.router.navigate(["../package-maintainers"]));
	}
}
