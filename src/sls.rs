#![deny(unused_must_use)]
use core::panic;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{self, Debug};
use std::fs::File;
use std::hash::Hash;
use std::mem::MaybeUninit;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::Instant;
use std::usize;

#[allow(non_camel_case_types)]
#[derive(Deserialize, Serialize, Debug, PartialEq, Clone, Copy, Default)]
pub enum NodeType {
    PULSE_BUTTON,
    #[default]
    TOGGLE_BUTTON,
    LIGHT_BULB,
    NOTE,
    D_FLIP_FLOP,
    SR_FLIP_FLOP,
    JK_FLIP_FLOP,
    T_FLIP_FLOP,
    SR_LATCH,
    MUX,
    DEMUX,
    FULL_ADDER,
    HALF_ADDER,
    AND_GATE,
    BUFFER_GATE,
    NAND_GATE,
    NOR_GATE,
    NOT_GATE,
    OR_GATE,
    XOR_GATE,
    XNOR_GATE,
    CLOCK,
    HIGH_CONSTANT,
    LOW_CONSTANT,
    FLASHLIGHT,
    RGB_LIGHT,
    SEVEN_SEGMENT_DISPLAY,
    DOT_MATRIX_DISPLAY_5X7,
    SPEAKER,
    VIBRATION,
    CHARGER_PLUGGED_SENSOR,
    BATTERY_LEVEL,
    LIGHT_SENSOR,
    MAGNETIC_FIELD_SENSOR,
    PROXIMITY_SENSOR,
    NOTIFICATION_LED,
    SEVEN_SEGMENT_DISPLAY_DECODER,
    INTEGRATED_CIRCUIT,
}
impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(PartialEq, Clone, Debug, Eq, Hash, Default, Ord, PartialOrd)]
pub struct ID(pub String);
impl Borrow<str> for ID {
    fn borrow(&self) -> &str {
        &self.0
    }
}
impl Serialize for ID {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}
struct IDVisitor;
use serde::de::{self, Error, Visitor};
impl<'de> Visitor<'de> for IDVisitor {
    type Value = ID;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("u32 or string")
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(ID((value).to_string()))
    }
    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(ID((value).to_string()))
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(ID(value.to_string()))
    }
    fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(ID(value))
    }
}
impl<'de> Deserialize<'de> for ID {
    fn deserialize<D>(deserializer: D) -> Result<ID, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(IDVisitor)
    }
}
#[derive(Debug, Clone, Default)]
pub struct InputState {
    pub state: bool,
    pub in_pin: usize,
}
#[derive(Deserialize, Serialize, Clone)]
pub struct Input {
    //might change from bool late
    #[serde(skip, default)]
    pub other_output: ComponentRef<bool>,
    #[serde(rename = "OTHER_CONNECTOR_ID")]
    pub other_pin: usize,
    #[serde(rename = "OTHER_COMPONENT")]
    pub other_id: ID,
    #[serde(rename = "CONNECTOR_ID")]
    pub in_pin: usize,
}
impl Debug for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //let local_bool;
        //let local_error;
        f.debug_struct("Input")
            //.field(
            //    "other_output",
            //    match self.other_output.get() {
            //        Ok(x) => {
            //            local_bool = x;
            //            &local_bool
            //        }
            //        Err(e) => {
            //            local_error = e;
            //            &local_error
            //        }
            //    },
            //)
            .field("other_pin", &self.other_pin)
            .field("other_id", &self.other_id)
            .field("in_pin", &self.in_pin)
            .finish()
    }
}

fn u64_iszero(num: &u64) -> bool {
    num == &0
}

// #[derive(Debug)]
// struct InputError {
//     com,
//     pin: usize,
// }
#[derive(Debug)]
enum NodeErrorType {
    Input(ComponentInputError, ID),
}
#[derive(Debug)]
struct NodeError {
    t: NodeErrorType,
}
impl std::fmt::Display for NodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.t {
            NodeErrorType::Input(i, id) => {
                write!(f, "Error with ID {}: ", &id.0)?;
                fmt::Display::fmt(&i, f)
                // f.write_fmt(format_args!("ID:{} is missing out pin {}", i.id.0, i.pin))
            }
        }
    }
}
impl std::error::Error for NodeError {}
#[derive(Clone)]
pub struct ComponentRef<T: Clone> {
    pub ptr: MaybeUninit<*const T>,
}
impl<T: Clone> Default for ComponentRef<T> {
    fn default() -> Self {
        Self {
            ptr: MaybeUninit::uninit(),
        }
    }
}

#[derive(Debug)]
pub enum ComponentInputError {
    IndexError(usize),
    ComponentNotExit,
}
impl std::error::Error for ComponentInputError {}
impl std::fmt::Display for ComponentInputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
impl<T: std::clone::Clone> ComponentRef<T> {
    pub fn new(ptr: *const T) -> Self {
        Self {
            ptr: MaybeUninit::new(ptr),
        }
    }
    pub fn get(&self) -> Result<T, ComponentInputError> {
        let ptr = unsafe { self.ptr.assume_init() };
        if ptr.is_null() {
            Err(ComponentInputError::ComponentNotExit)
        } else {
            let val: T = unsafe { &*ptr }.clone();
            Ok(val)
        }
        //match self.weak.upgrade() {
        //    None => Err(ComponentInputError::ComponentNotExit),
        //    Some(v) => match v.borrow().get(self.index) {
        //        None => Err(ComponentInputError::IndexError(self.index)),
        //        Some(b) => Ok(b.clone()),
        //    },
        //}
    }
}

#[derive(Deserialize, Serialize, Debug, Clone, Default)]
#[serde(rename_all = "UPPERCASE")]
pub struct Component {
    #[serde(rename = "TAG")]
    pub node_type: NodeType,
    //first we get the inputs then store them into input_states
    #[serde(default, skip_serializing)]
    pub inputs: Vec<Input>,
    //gets from inputs. Used for easier processing.
    #[serde(skip)]
    pub input_states: Vec<InputState>,
    #[serde(skip, default)]
    pub outputs: Vec<bool>,
    #[serde(skip)]
    pub next_outputs: Vec<bool>,
    #[serde(skip, default)]
    rising_edge_prev: bool, //used to detect rising edge inputs
    id: ID,
    pub x: f32,
    pub y: f32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    cid: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_of_in: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_of_out: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    size: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>, //used for NodeType::NOTE

    #[serde(skip)]
    pub ic_instance: Option<Circuit>,
    #[serde(default, skip_serializing_if = "u64_iszero")]
    pub period: u64,
}

impl Component {
    pub fn new(node_type: NodeType, label: Option<String>, id: String) -> Component {
        Component {
            node_type,
            label,
            id: ID(id),
            ..Default::default()
        }
    }
    pub fn at(self, x: f32, y: f32) -> Self {
        let mut n = self;
        n.x = x;
        n.y = y;
        n
    }
    pub fn get_id(&self) -> &ID {
        &self.id
    }
    pub fn get_size(&self) -> Option<usize> {
        self.size
    }
    pub fn set_size(&mut self, size: usize) {
        self.size = Some(size);
    }
    //of IC
    fn set_instance(&mut self, dependencies: &BTreeMap<String, Circuit>, deps_path: Option<&Path>) {
        if self.ic_instance.is_some() {
            return;
        }
        //this just does a shallow clone
        //we need to do a deep clone including outputs
        let cid = self.cid.as_ref().expect("cid");
        let original = &dependencies[cid];
        let mut new = original.clone();
        for comp in &mut new.components {
            comp.outputs = Vec::new();
        }
        if self.label.is_none() {
            self.label = Some(new.header.name.clone());
        }
        self.ic_instance = Some(new);
        //no idea if this makes a difference
        self.ic_instance
            .as_mut()
            .unwrap()
            .init_ic(dependencies, deps_path);
    }
    //call connect AFTER
    fn resize_output(&mut self) {
        let output_n = match self.num_of_out {
            Some(n) => n,
            None => {
                match self.node_type {
                    NodeType::INTEGRATED_CIRCUIT => {
                        let num_of_out = self.num_of_out.expect("num_of_out");
                        num_of_out
                    }
                    NodeType::DEMUX => self.size.expect("size of demux"),
                    NodeType::HALF_ADDER | NodeType::FULL_ADDER => 2,
                    NodeType::SEVEN_SEGMENT_DISPLAY_DECODER => 7,
                    NodeType::SEVEN_SEGMENT_DISPLAY
                    | NodeType::NOTE
                    | NodeType::DOT_MATRIX_DISPLAY_5X7
                    | NodeType::FLASHLIGHT
                    | NodeType::NOTIFICATION_LED
                    | NodeType::VIBRATION
                    | NodeType::RGB_LIGHT => 0,
                    //Q ~Q
                    NodeType::D_FLIP_FLOP
                    | NodeType::SR_LATCH
                    | NodeType::JK_FLIP_FLOP
                    | NodeType::SR_FLIP_FLOP
                    | NodeType::T_FLIP_FLOP => 2,
                    _ => 1,
                }
            }
        };
        let outputs = &mut self.outputs;
        outputs.resize(output_n, false);
        self.next_outputs.resize(output_n, false);
        match self.node_type {
            NodeType::D_FLIP_FLOP
            | NodeType::SR_LATCH
            | NodeType::JK_FLIP_FLOP
            | NodeType::SR_FLIP_FLOP
            | NodeType::T_FLIP_FLOP => {
                outputs[0] = false;
                outputs[1] = true;
                self.next_outputs[0] = false;
                self.next_outputs[1] = true;
            }
            NodeType::HIGH_CONSTANT => {
                outputs[0] = true;
                self.next_outputs[0] = true;
            }
            NodeType::LOW_CONSTANT => {
                outputs[0] = false;
                self.next_outputs[0] = false;
            }
            _ => {}
        }
        //also input_states
        //println!("inputs:{:#?} for {:?}", &self.inputs, self.node_type);
    }
    fn get_input(&self, input: &Input) -> Result<bool, NodeError> {
        match input.other_output.get() {
            Ok(b) => Ok(b),
            Err(e) => Err(NodeError {
                t: NodeErrorType::Input(e, input.other_id.clone()),
            }),
        }
    }
    fn set_input_state_in_pins(&mut self) {
        for (i, input) in self.inputs.iter().enumerate() {
            self.input_states[i].in_pin=input.in_pin;
        }
    }
    //returns true if any inputs changed
    fn get_inputs(&mut self) -> Result<bool, NodeError> {
        let mut changed: bool = false;
        for (i, input) in self.inputs.iter().enumerate() {
            let state = self.get_input(input)?;
            let input_state = &mut self.input_states[i];
            if state != input_state.state {
                changed = true;
                input_state.state = state;
            }
        }
        if self.node_type == NodeType::INTEGRATED_CIRCUIT {
            let instance: &mut Circuit = self.ic_instance.as_mut().unwrap();
            if instance.has_dynamic || changed {
                for comp in &mut instance.components {
                    comp.get_inputs()?;
                }
            }
        }
        Ok(changed)
    }
    fn next_output(&mut self, tick: u64) {
        match &self.node_type {
            NodeType::LIGHT_BULB => {
                for input in &self.input_states {
                    self.next_outputs[0] = input.state;
                }
            }
            NodeType::AND_GATE => {
                let mut out = self.input_states.iter().fold(true, |a,b|a&&b.state);
                if self.input_states.len() == 0 {
                    out = false;
                }
                self.next_outputs[0] = out;
            }
            NodeType::OR_GATE => {
                let mut out = false;
                for input in &self.input_states {
                    out |= input.state;
                }
                self.next_outputs[0] = out;
            }
            NodeType::XOR_GATE => {
                let mut out = true;
                for input in &self.input_states {
                    out ^= input.state;
                }
                self.next_outputs[0] = out;
            }
            NodeType::XNOR_GATE => {
                let mut out = true;
                for input in &self.input_states {
                    out ^= input.state;
                }
                self.next_outputs[0] = !out;
            }
            NodeType::NOR_GATE => {
                let mut out = false;
                for input in &self.input_states {
                    out |= input.state;
                }
                self.next_outputs[0] = !out;
            }
            NodeType::NOT_GATE => {
                let mut out = false;
                for input in &self.input_states {
                    out = !input.state;
                }
                self.next_outputs[0] = out;
            }
            NodeType::BUFFER_GATE => {
                let mut out = false;
                for input in &self.input_states {
                    out = input.state;
                }
                self.next_outputs[0] = out;
            }
            NodeType::HALF_ADDER => {
                let mut a = false;
                let mut b = false;
                for input in &self.input_states {
                    match input.in_pin {
                        0 => a = input.state,
                        1 => b = input.state,
                        _ => (),
                    }
                }
                self.next_outputs[0] = a ^ b;
                self.next_outputs[1] = a && b;
            }
            NodeType::FULL_ADDER => {
                let mut a = false;
                let mut b = false;
                let mut c = false;
                for input in &self.input_states {
                    match input.in_pin {
                        0 => a = input.state,
                        1 => b = input.state,
                        2 => c = input.state,
                        _ => (),
                    }
                }
                self.next_outputs[0] = a ^ b ^ c;
                self.next_outputs[1] = (a && b) || ((a ^ b) && c);
            }
            NodeType::DEMUX => {
                // order is sx-s0 then in
                let size = self.size.expect("size");
                let num_addr_pins = size.trailing_zeros() as usize;
                let mut pins = vec![false; num_addr_pins];
                let mut on = false;

                for input in &self.input_states {
                    if input.in_pin == num_addr_pins {
                        on = input.state;
                    } else {
                        pins[input.in_pin] = input.state;
                    }
                }
                let mut n = 0;
                let rev_pins = pins.iter().rev();
                for (i, pin) in rev_pins.enumerate() {
                    n |= *pin as u8 >> i;
                }
                for i in &mut self.next_outputs {
                    *i = false;
                }
                self.next_outputs[n as usize] = on;
            }
            NodeType::MUX => {
                // order is sx-s0 then in
                let size = self.size.expect("size");
                let num_addr_pins = size.trailing_zeros() as usize;
                let mut pins = vec![false; num_addr_pins];
                let mut input_pins = vec![false; size];

                for input in &self.input_states {
                    if input.in_pin < num_addr_pins {
                        pins[input.in_pin] = input.state;
                    } else {
                        input_pins[input.in_pin - (num_addr_pins)] = input.state;
                    }
                }
                let mut n = 0;
                let rev_pins = pins.iter().rev();
                for (i, pin) in rev_pins.enumerate() {
                    n |= *pin as u8 >> i;
                }
                for i in &mut self.next_outputs {
                    *i = false;
                }
                self.next_outputs[0] = input_pins[n as usize];
            }
            NodeType::CLOCK => {
                if (tick * 100) % self.period == 0 {
                    self.next_outputs[0] = !self.next_outputs[0];
                }
            }
            NodeType::INTEGRATED_CIRCUIT => {
                let instance = &mut self.ic_instance.as_mut().unwrap();
                //set/override instance's inputs
                for (i, input) in self.input_states.iter().enumerate() {
                    /*
                    println!(
                    "getting {:?} for {}",
                    input,
                    self.label.as_ref().unwrap_or(&"IC".to_string())
                    );
                    */
                    let out: bool = input.state;
                    let comp_index: usize = match instance.inputs.get(input.in_pin) {
                        Some(index) => *index,
                        None => {
                            panic!("IC name:{:?} id:{} failed to get input pin {} because it only has {:?}\ninput: {:#?}",self.label,self.cid.as_ref().unwrap(),input.in_pin,&instance.inputs,&self.inputs[i])
                        }
                    };
                    instance.components[comp_index].next_outputs[0] = out;
                    /*
                    println!(
                    "next_output {:?}\tset to {} from {}",
                    instance.components[comp_index].node_type, out, self.y
                    );
                    */
                }
                for comp in &mut instance.components {
                    comp.next_output(tick);
                }
                //set next out based on inner IC
                //println!("we are {:?}({}) {:?}",self.node_type,self.label.as_ref().unwrap(),instance.header.id);
                //println!(
                //    "we have {} inner componenents. and {} out pins. {} output pins and  {} comp outputs",
                //    instance.components.len(),
                //    self.next_outputs.len(),
                //    self.outputs.borrow().len(),
                //    instance.outputs.len(),
                //);
                //println!("{:#?}",&instance.outputs);
                for i in 0..instance.outputs.len() {
                    //println!("get comp {}", instance.outputs[i]);
                    let comp_index: usize = instance.outputs[i];
                    //println!("{:?} output",instance.components[comp_index].node_type);
                    self.next_outputs[i] = instance.components[comp_index].next_outputs[0];
                }
            }
            NodeType::SR_LATCH => {
                let mut set = false;
                let mut reset = false;
                for input in &self.input_states {
                    let state = input.state;
                    match input.in_pin {
                        0 => {
                            set = state;
                        }
                        1 => {
                            reset = state;
                        }
                        n => {
                            panic!("tried to acess input {} of {:?}", n, self.node_type);
                        }
                    }
                }
                match (set, reset) {
                    (true, false) => {
                        self.next_outputs[0] = true;
                        self.next_outputs[1] = false;
                    }
                    (false, true) => {
                        self.next_outputs[0] = false;
                        self.next_outputs[1] = true;
                    }
                    (true, true) => {
                        self.next_outputs[0] = true;
                        self.next_outputs[1] = false;
                    }
                    (false, false) => {}
                }
            }
            NodeType::T_FLIP_FLOP => {
                //Q and ~Q
                if !self.next_outputs[0] && !self.next_outputs[1] {
                    self.next_outputs[1] = true; //~Q
                }

                let mut preset: bool = false;
                let mut t: bool = false;
                let mut clock: bool = false;
                let mut clear: bool = false;
                for input in &self.input_states {
                    let state = input.state;
                    match input.in_pin {
                        0 => {
                            preset = state;
                        }
                        1 => {
                            t = state;
                        }
                        2 => {
                            clock = state;
                        }
                        3 => {
                            clear = state;
                        }
                        n => {
                            panic!("tried to acess input {} of T_FLIP_FLOP", n);
                        }
                    }
                }
                match (preset, clear) {
                    (false, false) => {
                        (self.next_outputs[0], self.next_outputs[1]) = (false, true);
                    }
                    (true, false) => {
                        (self.next_outputs[0], self.next_outputs[1]) = (false, true);
                    }
                    (false, true) => {
                        (self.next_outputs[0], self.next_outputs[1]) = (true, false);
                    }
                    (true, true) => {
                        match t {
                            true => {
                                if !self.rising_edge_prev && clock {
                                    //on rising edge of clock
                                    (self.next_outputs[0], self.next_outputs[1]) =
                                        (self.next_outputs[1], self.next_outputs[0]);
                                }
                            }
                            false => (),
                        }
                    }
                }
                self.rising_edge_prev = clock;
            }
            NodeType::D_FLIP_FLOP => {
                let mut data: bool = false;
                let mut clock: bool = false;
                let mut set: bool = false;
                let mut reset: bool = false;
                //for some reason out of order
                for input in &self.input_states {
                    let state = input.state;
                    match input.in_pin {
                        0 => {
                            set = state;
                        }
                        1 => {
                            reset = state;
                        }
                        2 => {
                            data = state;
                        }
                        3 => {
                            clock = state;
                        }
                        n => {
                            panic!("tried to acess input {} of D_FLIP_FLOP", n);
                        }
                    }
                }
                match (set, reset) {
                    (false, false) => {
                        (self.next_outputs[0], self.next_outputs[1]) = (false, true);
                    }
                    (true, false) => {
                        (self.next_outputs[0], self.next_outputs[1]) = (false, true);
                    }
                    (false, true) => {
                        (self.next_outputs[0], self.next_outputs[1]) = (true, false);
                    }
                    (true, true) => {
                        if !self.rising_edge_prev && clock {
                            //on rising edge of clock
                            self.next_outputs[0] = data;
                            self.next_outputs[1] = !data;
                        }
                    }
                }
                self.rising_edge_prev = clock;
            }
            NodeType::SEVEN_SEGMENT_DISPLAY_DECODER => {
                //not gonna actually simulate, just gonna show input number
            }
            NodeType::SEVEN_SEGMENT_DISPLAY => {}
            NodeType::NOTE => {}
            NodeType::PULSE_BUTTON | NodeType::TOGGLE_BUTTON => {}
            NodeType::HIGH_CONSTANT | NodeType::LOW_CONSTANT => {}
            _ => {
                todo!("{:?}", &self.node_type);
            }
        }
    }
    fn update_output(&mut self) {
        //self.outputs.borrow_mut().clone_from(&self.next_outputs);
        let outputs = &mut self.outputs;
        for (i, output) in self.next_outputs.iter().enumerate() {
            outputs[i] = *output;
        }
        if let Some(instance) = &mut self.ic_instance.as_mut() {
            for comp in &mut instance.components {
                comp.update_output();
            }
        }
    }
}

#[derive(Deserialize, Default, Debug)]
#[serde(rename_all = "UPPERCASE")]
enum CircuitType {
    Project,
    #[default]
    Ic,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Wire {
    #[serde(rename = "S")]
    to: WireID,
    #[serde(rename = "E")]
    from: WireID,
}
impl Wire {
    pub fn new(from: ID, from_out: usize, to: ID, to_in: usize) -> Wire {
        Wire {
            from: WireID(from, from_out),
            to: WireID(to, to_in),
        }
    }
}
#[derive(PartialEq, Clone, Debug, Eq, Hash, Default)]
struct WireID(ID, usize);

impl<'de> Deserialize<'de> for WireID {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_string(WireIDVisitor)
    }
}
impl Serialize for WireID {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("{}:{}", self.0 .0, self.1))
    }
}
#[derive(Debug)]
enum WireError {
    Message(String),
}
impl std::error::Error for WireError {}
impl std::fmt::Display for WireError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WireError::Message(m) => write!(f, "WireID \"{}\"", m),
        }
    }
}
impl de::Error for WireError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        print!("WireError: {}", msg);
        WireError::Message("Error".to_string())
    }
}

struct WireIDVisitor;

impl<'de> Visitor<'de> for WireIDVisitor {
    type Value = WireID;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("String")
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: Error,
    {
        self.visit_str(&v)
    }
    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        self.visit_str(v)
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        let mut iter = v.split(':');
        let id;
        let pin;
        match iter.next() {
            Some(id_str) => {
                id = ID(id_str.to_string());
            }
            None => {
                return Err(de::Error::custom(format!("Expected an ID but got {}", v)));
            }
        }
        match iter.next() {
            Some(pin_str) => {
                pin = match usize::from_str(pin_str) {
                    Err(e) => {
                        return Err(de::Error::custom(format!(
                            "Expected a pin number after ID but got {} ({})",
                            pin_str, e
                        )));
                    }
                    Ok(pin_n) => pin_n,
                };
            }
            None => {
                return Err(de::Error::custom(format!(
                    "Expected a pin number after ID but got {}",
                    v
                )));
            }
        }
        Ok(WireID(id, pin))
    }
}
#[derive(Deserialize, Serialize, Default, Debug, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub struct Header {
    pub name: String,
    app_version: usize,
    #[serde(default)]
    pub id: ID,
    pub color: Option<Color>,
    //not in latest version
    //#[serde(rename = "TYPE", default, skip_serializing)]
    //circ_type: CircuitType,
}

#[derive(Default, Clone, Copy)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}
impl Debug for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_string())
    }
}
impl ToString for Color {
    fn to_string(&self) -> String {
        format!("#{:02x}{:02x}{:02x}", self.r, self.g, self.b)
    }
}
impl<'de> Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_string(ColorVisitor)
    }
}
impl Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

struct ColorVisitor;
impl<'de> Visitor<'de> for ColorVisitor {
    type Value = Color;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("Color String")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        match v.strip_prefix("#") {
            None => return Err(de::Error::custom("expected color to start with #")),
            Some(v) => {
                if v.len() != 6 {
                    return Err(de::Error::custom(format!(
                        "Color doesn't have 6 digits: \"{}\"",
                        v
                    )));
                }
                let color_num: u32 = match u32::from_str_radix(v, 16) {
                    Ok(v) => v,
                    Err(e) => return Err(de::Error::custom(e)),
                };
                Ok(Color {
                    r: (color_num >> (8 * 2)) as u8,
                    g: (color_num >> (8 * 1)) as u8,
                    b: (color_num >> (8 * 0)) as u8,
                })
            }
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Default, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub struct Circuit {
    #[serde(skip)]
    uris: HashMap<String, String>,
    pub header: Header,
    pub components: Vec<Component>,
    #[serde(default)]
    pub dependencies: BTreeMap<String, Circuit>,
    //indicies of inputs
    #[serde(skip)]
    pub inputs: Vec<usize>,
    #[serde(skip)]
    pub outputs: Vec<usize>,
    #[serde(skip)]
    pub tick_count: u64,
    #[serde(default)]
    wires: Vec<Wire>,
    #[serde(skip)]
    pub begin: Option<Instant>,
    //components that change outputs
    //even when inputs haven't changed
    #[serde(skip)]
    pub has_dynamic: bool,
}
impl Circuit {
    pub fn new(
        name: String,
        id: String,
        components: Vec<Component>,
        wires: Vec<Wire>,
        color: Option<Color>,
    ) -> Self {
        Circuit {
            header: Header {
                name,
                app_version: 158,
                id: ID(id),
                color,
            },
            components,
            wires,
            ..Default::default()
        }
    }
}
/// returns true when added and false when already in hashmap
fn add_dep<'hm>(dependencies: &'hm mut BTreeMap<String, Circuit>, cid: &String, uri: &str) -> bool {
    //let result: Option<&IC> = self.dependencies.get(cid);
    if !dependencies.contains_key(cid) {
        //open URI
        match File::open(uri) {
            Err(e) => {
                panic!("Couldn't find URI:\"{}\"| {}", uri, e);
            }

            Ok(f) => {
                let c: Circuit = match serde_json::from_reader(std::io::BufReader::new(f)) {
                    Err(e) => {
                        panic!("Couldn't find URI:\"{}\"| {}", uri, e);
                    }
                    Ok(c) => c,
                };
                println!("adding {}({})", uri, cid);
                dependencies.insert(cid.clone(), c);

                //Some(&dependencies[cid])
                true
            }
        }
    } else {
        //None
        false
    }
}
impl Circuit {
    //go thru each dep and init (which will add more deps)
    fn add_deps(&mut self, path: &Path) {
        //URIs
        let mut deps_path = PathBuf::from(path);
        deps_path.push("URIs.json");
        if let Ok(s) = std::fs::read_to_string(&deps_path) {
            self.uris = serde_json::from_str(&s).unwrap();
            println!("added URIs.json: {:?}", &self.uris);
        }
        println!("add componentts' deps");
        //add ones from coomponents
        self.components
            .iter()
            .filter(|c| c.node_type == NodeType::INTEGRATED_CIRCUIT)
            .for_each(|comp| {
                let cid = comp.cid.as_ref().unwrap();
                if !self.dependencies.contains_key(cid) {
                    let mut p = PathBuf::from(path);
                    p.push(
                        self.uris
                            .get(cid)
                            .unwrap_or(comp.label.as_ref().unwrap_or(cid)),
                    );
                    add_dep(&mut self.dependencies, cid, p.to_str().unwrap());
                }
            });
        println!("add subdeps");
    }
    fn connect(&mut self) {
        //resize output vecs based on the type
        if self.header.id.0 == "b2f78426-068d-4d99-8e64-dec6fe2ff0a7".to_owned() {
            //println!("Here!\n{:#?}!!!!!!!!!!", &self);
        }
        for comp in &mut self.components {
            comp.resize_output();
        }

        let mut ids: HashMap<ID, Vec<*const bool>> = HashMap::with_capacity(self.components.len());
        //println!("components: {:?}", (&self.components).as_ptr());
        //so the components are different
        //how are the outputs or inputs copied to the next IC?
        for comp in self.components.iter() {
            let mut v:Vec<*const bool> = Vec::with_capacity(comp.outputs.len());
            for output in comp.outputs.iter() {
                v.push(output as *const bool);
            }
            ids.insert(comp.id.clone(),v);
        }
        //for (id, r) in &ids {
        //    println!("{}: {:#?}", id.0, r.upgrade().unwrap().as_ptr());
        //}
        for comp in &mut self.components {
            //println!("inputs: {:?}", &comp.inputs);
            comp.inputs.clear();
        }
        for wire in &self.wires {
            let comp = self
                .components
                .iter_mut()
                .find(|comp| comp.id == wire.to.0)
                .unwrap();
            comp.inputs.push(Input {
                other_output: ComponentRef::new(ids.get(&wire.from.0).unwrap()[wire.from.1]),
                other_pin: wire.from.1,
                other_id: wire.from.0.clone(),
                in_pin: wire.to.1,
            });
        }
        for comp in &mut self.components {
            comp.input_states
                .resize(comp.inputs.len(), InputState::default());
            comp.set_input_state_in_pins();
            //println!(
            //    "resizing({:?}): input_states: {:?} inputs:{:?}",
            //    &comp.node_type, &comp.input_states, &comp.inputs
            //);
        }
    }
    fn check_dynamic(&mut self) {
        for comp in &mut self.components {
            match comp.node_type {
                NodeType::CLOCK => {
                    self.has_dynamic = true;
                    return;
                }
                NodeType::INTEGRATED_CIRCUIT => {
                    let instance = comp.ic_instance.as_mut().unwrap();
                    instance.check_dynamic();
                    if instance.has_dynamic {
                        self.has_dynamic = true;
                        return;
                    }
                }
                _ => (),
            }
        }
        for comp in &self.components {
            if self.check_circular(comp, comp.get_id(), 4) {
                self.has_dynamic = true;
                return;
            }
        }
    }
    pub fn get_speed(&self) -> u128 {
        //use begin and tick_count
        let now = Instant::now();
        let passed = now.duration_since(self.begin.unwrap());
        let avg_mspt = passed.as_millis() / self.tick_count as u128;
        return avg_mspt;
    }
    fn check_circular(&self, comp: &Component, original_id: &ID, num_iters: usize) -> bool {
        //check circular input
        for input in &comp.inputs {
            if input.other_id == comp.id || &input.other_id == original_id {
                return true;
            }
            if num_iters > 0 {
                if self.check_circular(
                    &self
                        .components
                        .iter()
                        .find(|c| c.get_id() == &input.other_id)
                        .unwrap(),
                    original_id,
                    num_iters - 1,
                ) {
                    return true;
                }
            }
        }
        return false;
    }
    pub fn init_circ(&mut self, deps_path: Option<&Path>) {
        //add depependencies
        //go thru URIs.json and add each one to dependencies that we need
        if let Some(deps_path) = deps_path {
            self.add_deps(deps_path);
        }
        for comp in self
            .components
            .iter_mut()
            .filter(|node| node.node_type == NodeType::INTEGRATED_CIRCUIT)
        {
            comp.set_instance(&self.dependencies, deps_path);
        }
        //coonnect components
        self.connect();
        self.check_dynamic();
        //println!("wires: {:?}", self.wires);
        self.get_io_indexes_top();
        self.begin = Some(Instant::now());
    }
    pub fn init_ic(&mut self, dependencies: &BTreeMap<String, Circuit>, deps_path: Option<&Path>) {
        //add depependencies
        //go thru URIs.json and add each one to dependencies that we need
        //setup dependencies to be cloned
        for comp in self
            .components
            .iter_mut()
            .filter(|node| node.node_type == NodeType::INTEGRATED_CIRCUIT)
        {
            comp.set_instance(dependencies, deps_path);
        }
        //coonnect components
        self.connect();
        //println!("wires: {:?}", self.wires);
        self.get_io_indexes();
    }
    //1 tick = 100 ms
    pub fn tick(&mut self) {
        for i in 0..self.components.len() {
            match self.components[i].get_inputs() {
                Err(e) => {
                    eprintln!("Input Error!");
                    match &e.t {
                        NodeErrorType::Input(i, id) => {
                            if let Some(c) = self.components.iter().find(|c| &c.id == id) {
                                eprintln!(
                                    "other: {} {:?} {:?}: {}",
                                    &c.id.0, c.label, c.node_type, i
                                );
                            } else {
                                eprintln!("couldn't get other component with id: {}: {}", &id.0, i);
                            }
                        }
                    }
                    panic!("Node errot :( ){}", e);
                }
                Ok(b) => {
                    if self.has_dynamic || b {
                        self.components[i].next_output(self.tick_count);
                    }
                }
            }
        }
        for component in &mut self.components {
            component.update_output();
        }
        self.tick_count += 1;
    }
    fn sort_comps(&mut self) {
        self.components
            .sort_by(|comp1, comp2| comp1.y.partial_cmp(&comp2.y).unwrap());
    }
    fn get_io_indexes(&mut self) {
        self.sort_comps();
        for i in 0..self.components.len() {
            match self.components[i].node_type {
                NodeType::PULSE_BUTTON | NodeType::TOGGLE_BUTTON => {
                    self.inputs.push(i);
                }
                NodeType::LIGHT_BULB => {
                    self.outputs.push(i);
                }
                _ => (),
            }
        }
    }
    pub fn get_io_indexes_top(&mut self) {
        self.sort_comps();

        for i in 0..self.components.len() {
            match self.components[i].node_type {
                NodeType::PULSE_BUTTON | NodeType::TOGGLE_BUTTON => {
                    self.inputs.push(i);
                }
                NodeType::LIGHT_BULB | NodeType::SEVEN_SEGMENT_DISPLAY_DECODER => {
                    self.outputs.push(i);
                }
                _ => (),
            }
        }
    }
}
