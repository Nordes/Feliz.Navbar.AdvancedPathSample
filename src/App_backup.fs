module App

open Feliz
open Feliz.Router
open Elmish
open Browser.Types


type State = { CurrentUrl : string list }

type Msg =
    | UrlChanged of string list
    | NavigateToUsers
    | NavigateToUser of int
    | NavigateTo of string

let goToUrl (dispatch: Msg -> unit) (href: string) (e: MouseEvent) =
    // disable full page refresh
    e.preventDefault()
    // dispatch msg
    dispatch (NavigateTo href)

let init() = { CurrentUrl = Router.currentPath() }, Cmd.none

let update msg state =
    match msg with
    | UrlChanged segments -> { state with CurrentUrl = segments }, Cmd.none
    // notice here the use of the command Router.navigate
    | NavigateToUsers -> state, Router.navigatePath("users")
    // Router.navigate with query string parameters
    | NavigateToUser userId -> state, Router.navigatePath("users", [ "id", userId ])
  
    | NavigateTo href -> state, Router.navigatePath(href)

let render state dispatch =

    let currentPage =
        match state.CurrentUrl with
        | [ ] ->
            Html.div [
                Html.h1 "Home"
                Html.button [
                    prop.text "Navigate to users"
                    prop.onClick (fun _ -> dispatch NavigateToUsers)
                ]
                Html.a [
                    // prop.href (Router.formatPath("users"))
                    prop.onClick (goToUrl dispatch (Router.formatPath("users")))
                    prop.href ""
                    prop.text "Users link"
                ]
            ]
        | [ "users" ] ->
            Html.div [
                Html.h1 "Users page"
                Html.button [
                    prop.text "Navigate to User(10)"
                    prop.onClick (fun _ -> dispatch (NavigateToUser 10))
                ]
                Html.a [
                    // prop.href (Router.formatPath("users", ["id", 10]))
                    prop.onClick (goToUrl dispatch (Router.formatPath("users", ["id", 10])))
                    prop.href ""
                    prop.text "Single User link"
                ]
            ]

        | [ "users"; Route.Query [ "id", Route.Int userId ] ] ->
            Html.h1 (sprintf "Showing user %d" userId)

        | _ ->
            Html.h1 "Not found"

    Router.router [
        Router.pathMode
        Router.onUrlChanged (UrlChanged >> dispatch)
        Router.application currentPage
    ]