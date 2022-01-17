import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import {LoginComponentComponent} from "./login-component/login-component.component";
import {RegisterComponentComponent} from "./register-component/register-component.component";
import {UploadPaperComponent} from "./upload-paper/upload-paper.component";
import {AddConferenceComponent} from "./add-conference/add-conference.component";

const routes: Routes = [
  {path: 'login', component: LoginComponentComponent},
  {path: 'register', component: RegisterComponentComponent},
  {path: 'upload', component: UploadPaperComponent},
  {path: 'add-conference', component: AddConferenceComponent}
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
