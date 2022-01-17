import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { MaintainerModel, emptyMaintainer } from "../maintainers/shared/maintainer.model";
import { MaintainerService } from "../maintainers/shared/maintainer.service";

@Component({
	selector: "app-add-maintainer",
	templateUrl: "./add-maintainer.component.html",
	styleUrls: ["./add-maintainer.component.css"],
	providers: [MaintainerService]
})
export class AddMaintainerComponent implements OnInit {
	maintainer: MaintainerModel = emptyMaintainer;

	constructor(private router: Router, private service: MaintainerService) { }

	ngOnInit(): void { }

	async onAdd() {
		this.service.addMaintainer(this.maintainer).subscribe(() => this.router.navigate(["../maintainers"]));
	}
}
