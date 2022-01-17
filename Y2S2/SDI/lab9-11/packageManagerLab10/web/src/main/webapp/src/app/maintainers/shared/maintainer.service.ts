import { Injectable } from '@angular/core';
import { HttpClient } from "@angular/common/http";
import { Observable } from "rxjs";
import { MaintainerModel } from './maintainer.model';

@Injectable()
export class MaintainerService {
  private maintainersUrl = 'http://localhost:8080/web-1.0-SNAPSHOT/api/maintainers';

  constructor(private httpClient: HttpClient) { }

  getMaintainers(): Observable<MaintainerModel[]> {
    return this.httpClient.get<Array<MaintainerModel>>(this.maintainersUrl);
  }

  addMaintainer(maintainer: MaintainerModel): Observable<MaintainerModel> {
    return this.httpClient.post<MaintainerModel>(this.maintainersUrl, maintainer);
  }

  updateMaintainer(maintainer: MaintainerModel): Observable<MaintainerModel> {
    return this.httpClient.put<MaintainerModel>(`${this.maintainersUrl}/${maintainer.id}`, maintainer);
  }

  deleteMaintainer(maintainer: MaintainerModel): Observable<any> {
    return this.httpClient.delete(`${this.maintainersUrl}/${maintainer.id}`);
  }
}
