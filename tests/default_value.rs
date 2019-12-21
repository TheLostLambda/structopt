use structopt::StructOpt;

#[test]
fn auto_default_value() {
    #[derive(StructOpt, PartialEq, Debug)]
    struct Opt {
        #[structopt(default_value)]
        arg: i32,
    }
    assert_eq!(Opt { arg: 0 }, Opt::from_iter(&["test"]));
    assert_eq!(Opt { arg: 1 }, Opt::from_iter(&["test", "1"]));
}
