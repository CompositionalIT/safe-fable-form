module Index

open Elmish
open Fable.Form.Simple
open Fable.Form.Simple.Bulma
open Fable.Remoting.Client
open Shared

type Values = { Todo: string }
type Form = Form.View.Model<Values>
type Model = { Todos: Todo list; Form: Form }

type Msg =
    | GotTodos of Todo list
    | FormChanged of Form
    | AddTodo of string
    | AddedTodo of Todo

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Form = Form.View.idle { Todo = "" } }

    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | FormChanged form -> { model with Form = form }, Cmd.none
    | AddTodo todo ->
        let todo = Todo.create todo
        model, Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo
    | AddedTodo todo ->
        let newModel =
            { model with
                  Todos = model.Todos @ [ todo ]
                  Form =
                      { model.Form with
                            State = Form.View.Success "Todo added"
                            Values = { model.Form.Values with Todo = "" } } }
        newModel, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let form : Form.Form<Values, Msg, _> =
    let todoField =
        Form.textField
            {
                Parser = Ok
                Value = fun values -> values.Todo
                Update = fun newValue values -> { values with Todo = newValue }
                Error = fun _ -> None
                Attributes =
                    {
                        Label = "New todo"
                        Placeholder = "What needs to be done?"
                        HtmlAttributes = []
                    }
            }

    Form.succeed AddTodo
    |> Form.append todoField

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Form.View.asHtml
            {
                Dispatch = dispatch
                OnChange = FormChanged
                Action = Form.View.Action.SubmitOnly "Add"
                Validation = Form.View.Validation.ValidateOnSubmit
            }
            form
            model.Form
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "safe_fable_form"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]