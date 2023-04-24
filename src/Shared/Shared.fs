namespace Shared

open System

type Todo = { Id: Guid; HighPriority: bool; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create highPriority (description: string) =
        { Id = Guid.NewGuid()
          HighPriority = highPriority
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }