import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PackagesComponent } from './packages/packages.component';
import { MaintainersComponent } from './maintainers/maintainers.component';
import { AddPackageComponent } from './add-package/add-package.component';
import { AddMaintainerComponent } from './add-maintainer/add-maintainer.component';
import { UpdatePackageComponent } from './update-package/update-package.component';
import { UpdateMaintainerComponent } from './update-maintainer/update-maintainer.component';
import { DeletePackageComponent } from './delete-package/delete-package.component';
import { DeleteMaintainerComponent } from './delete-maintainer/delete-maintainer.component';

const routes: Routes = [
  { path: 'packages', component : PackagesComponent },
  { path: 'add-package', component : AddPackageComponent },
  { path: 'update-package', component : UpdatePackageComponent },
  { path: 'delete-package', component : DeletePackageComponent },
  { path: 'maintainers', component : MaintainersComponent },
  { path: 'add-maintainer', component : AddMaintainerComponent },
  { path: 'update-maintainer', component : UpdateMaintainerComponent },
  { path: 'delete-maintainer', component : DeleteMaintainerComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})

export class AppRoutingModule { }
