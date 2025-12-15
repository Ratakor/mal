module ListTraverse = List.Traverse (struct
  type 'a t = ('a, string) result

  let return = Result.return
  let ( >>= ) = Result.( >>= )
end)
