Change log

01 Initial project

  - Android fragment, https://developer.android.com/guide/components/fragments
  - Android navigation component, https://developer.android.com/guide/navigation/navigation-getting-started

02 Refactor project

- Use ItemListFragment, ItemEditFragment
- Add logs

03 Android lists

- Recycler view, https://developer.android.com/guide/topics/ui/layout/recyclerview
- View models, https://developer.android.com/topic/libraries/architecture/viewmodel
- Guide to app architecture, https://developer.android.com/jetpack/guide

04 View models & live data

- Live data, https://developer.android.com/topic/libraries/architecture/livedata

05 Repository

- Repository, https://developer.android.com/jetpack/guide
- OkHttp, https://square.github.io/okhttp/
- Retrofit, https://square.github.io/retrofit/

06 Master-detail, online

- Show a list of items
- Create/update item
- Use a REST service

07 Refactor to a modular structure

- Guide to app architecture, https://developer.android.com/jetpack/guide
- Recommended structure
    - core/common
    - module (e.g. todo)
        - feature (e.g. items)
            - fragments, adapters, view models (e.g. ItemListFragment, ItemListAdapter, ItemListViewModel)
        - data
            - entities (e.g. Item)
            - repositories (e.g. ItemRepository)
            - local
                - data aceess objects
            - remote
                - apis/rest clients

* 08 - Authentication

* 09 - Room
