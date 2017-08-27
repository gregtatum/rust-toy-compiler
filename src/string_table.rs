pub type StringIndex = usize;

pub struct StringTable {
    strings: Vec<String>
}

impl StringTable {
    pub fn new() -> StringTable {
        StringTable {
            strings: Vec::new()
        }
    }

    pub fn take_string(&mut self, string: String) -> StringIndex {
        match self.strings.iter().position(|s| *s == string) {
            Some(index) => index,
            None => {
                let index = self.strings.len();
                self.strings.push(string);
                index
            }
        }
    }

    pub fn index(&mut self, string: &String) -> StringIndex {
        match self.strings.iter().position(|s| s == string) {
            Some(index) => index,
            None => {
                let index = self.strings.len();
                self.strings.push(string.to_string());
                index
            }
        }
    }

    pub fn string(&self, index: StringIndex) -> Option<&String> {
        self.strings.get(index)
    }
}
