let splitStringInLines (s:string) = s.Split([|System.Environment.NewLine|],System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x->x.Trim())
