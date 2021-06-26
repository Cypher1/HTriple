use crate::cli_options::Options;
use crate::database::{Compiler, DB};
use crate::errors::TError;
use crate::components::*;
use specs::prelude::*;
use std::fs::File;
use std::io::Read;
use std::sync::Arc;

use crate::ast::Visitor;

#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;

struct FindSymbols;

impl<'a> System<'a> for FindSymbols {
    type SystemData = (
        ReadStorage<'a, Token>,
        WriteStorage<'a, IsSymbol>,
        WriteStorage<'a, IsAbstraction>,
        WriteStorage<'a, IsDefinition>,
        WriteStorage<'a, HasValue>,
        Entities<'a>,
    );

    fn run(&mut self, (tokens, mut syms, mut abs, mut lets, _values, entities): Self::SystemData) {}
}

fn type_analysis_setup(world: &mut World, filename: &str) -> Result<(), TError> {
    let mut db: DB = DB::default();
    // TODO: Shouldn't need this anymore
    db.set_options((*world.read_resource::<Options>()).clone());

    // use crate::parser::parse_string;
    let mut contents = String::new();
    let mut file = File::open(filename.to_owned())?;
    file.read_to_string(&mut contents)?;
    let module_name = db.module_name(filename.to_owned());
    let contents = Arc::new(contents);
    db.set_file(filename.to_owned(), Ok(contents.clone()));

    use crate::experimental::definition_finder::DefinitionFinder;

    {
        if db.debug_level() > 0 {
            eprintln!("building symbol table >> {}", &filename);
        }
        let mut symbol_table_builder = DefinitionFinder::new(world);
        let result = symbol_table_builder.visit_root(&db, &module_name);
        dbg!(result)?;
    }
    {
        if db.debug_level() > 0 {
            eprintln!("look up definitions >> {}", &filename);
        }
        let mut def_finder = DefinitionFinder::new(world);
        let result = def_finder.visit_root(&db, &module_name);
        dbg!(result)?;
    }

    /*toks.iter().map(|tok| {
        world
            .create_entity()
            .with(Token {
                token: tok.tok_type.clone(),
                value: tok.value.clone(),
            })
            .with(tok.pos.clone())
            .build()
    });*/

    Ok(())
}

fn type_analysis(world: &mut World) -> Result<(), TError> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dummy_source_proc() -> Result<(), TError> {
        let filename = "examples/fibAcc.tk";

        let mut world = World::new();
        world.register::<AtLoc>();
        world.register::<Token>();
        world.register::<HasErrors>();
        world.register::<IsSymbol>();
        world.register::<IsDefinition>();
        world.register::<IsAbstraction>();
        world.register::<HasValue>();

        world.insert(Options::default().with_file(filename)); 

        type_analysis_setup(&mut world, filename)?;
        type_analysis(&mut world)?;

        let mut symbol_finder = FindSymbols;
        symbol_finder.run_now(&world);

        use specs::DispatcherBuilder;

        let mut dispatcher_builder = DispatcherBuilder::new();
        dispatcher_builder.add(symbol_finder, "find_syms", &[]);
        let mut dispatcher = dispatcher_builder.build();

        dispatcher.setup(&mut world);
        dispatcher.dispatch(&mut world);

        Ok(())
    }
}
