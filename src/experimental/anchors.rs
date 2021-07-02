#[cfg(test)]
mod tests {
    use anchors::{singlethread::Engine, expert::Var};
    use anchors::expert::MultiAnchor;

    #[test]
    fn doc_test() {
        let mut engine = Engine::new();

        // create a couple `Var`s
        let (my_name, my_name_updater) = {
            let var = Var::new("Bob".to_string());
            (var.watch(), var)
        };
        let (my_unread, my_unread_updater) = {
            let var = Var::new(999usize);
            (var.watch(), var)
        };

        // `my_name` is a `Var`, our first type of `Anchor`. we can pull an `Anchor`'s value out with our `engine`:
        assert_eq!(&engine.get(&my_name), "Bob");
        assert_eq!(engine.get(&my_unread), 999);

        // we can create a new `Anchor` from another one using `map`. The function won't actually run until absolutely necessary.
        // also feel free to clone an `Anchor` — the clones will all refer to the same inner state
        let my_greeting = my_name.clone().map(|name| {
            println!("calculating name!");
            format!("Hello, {}!", name)
        });
        assert_eq!(engine.get(&my_greeting), "Hello, Bob!"); // prints "calculating name!"

        // we can update a `Var` with its updater. values are cached unless one of its dependencies changes
        assert_eq!(engine.get(&my_greeting), "Hello, Bob!"); // doesn't print anything
        my_name_updater.set("Robo".to_string());
        assert_eq!(engine.get(&my_greeting), "Hello, Robo!"); // prints "calculating name!"

        // a `map` can take values from multiple `Anchor`s. just use tuples:
        let header = (&my_greeting, &my_unread)
            .map(|greeting, unread| format!("{} You have {} new messages.", greeting, unread));
        assert_eq!(
            engine.get(&header),
            "Hello, Robo! You have 999 new messages."
        );

        // just like a future, you can dynamically decide which `Anchor` to use with `then`:
        let (insulting_name, _) = {
            let var = Var::new("Lazybum".to_string());
            (var.watch(), var)
        };
        let dynamic_name = my_unread.then(move |unread| {
            // only use the user's real name if the have less than 100 messages in their inbox
            if *unread < 100 {
                my_name.clone()
            } else {
                insulting_name.clone()
            }
        });
        assert_eq!(engine.get(&dynamic_name), "Lazybum");
        my_unread_updater.set(50);
        assert_eq!(engine.get(&dynamic_name), "Robo");
    }
}
