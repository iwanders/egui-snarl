use egui::{Color32, Pos2, Style, Ui};

use crate::{InPin, NodeId, OutPin, Snarl};

use super::pin::PinInfo;

/// SnarlViewer is a trait for viewing a Snarl.
///
/// It can extract necessary data from the nodes and controls their
/// response to certain events.
pub trait SnarlViewer<T> {
    /// Returns title of the node.
    fn title(&mut self, node: &T) -> String;

    /// Checks if node has something to show in body - between input and output pins.
    fn has_body(&mut self, node: &T) -> bool {
        let _ = node;
        false
    }

    /// Checks if node has something to show in footer - below pins and body.
    fn has_footer(&mut self, node: &T) -> bool {
        let _ = node;
        false
    }

    /// Checks if node has something to show in on-hover popup.
    fn has_on_hover_popup(&mut self, node: &T) -> bool {
        let _ = node;
        false
    }

    /// Renders the node's header.
    fn show_header(
        &mut self,
        node: NodeId,
        inputs: &[InPin],
        outputs: &[OutPin],
        ui: &mut Ui,
        scale: f32,
        snarl: &mut Snarl<T>,
    ) {
        let _ = (inputs, outputs, scale);
        ui.label(self.title(&snarl[node]));
    }

    /// Returns number of output pins of the node.
    fn outputs(&mut self, node: &T) -> usize;

    /// Returns number of input pins of the node.
    fn inputs(&mut self, node: &T) -> usize;

    /// Renders the node's input pin.
    fn show_input(&mut self, pin: &InPin, ui: &mut Ui, scale: f32, snarl: &mut Snarl<T>)
        -> PinInfo;

    /// Obtain pin info for vertical inputs, these cannot draw elements themselves.
    fn vertical_input(&mut self, _pin: &InPin, _snarl: &mut Snarl<T>) -> Option<PinInfo> {
        None
    }

    /// Renders the node's output pin.
    fn show_output(
        &mut self,
        pin: &OutPin,
        ui: &mut Ui,
        scale: f32,
        snarl: &mut Snarl<T>,
    ) -> PinInfo;

    /// Renders the node's body.
    fn show_body(
        &mut self,
        node: NodeId,
        inputs: &[InPin],
        outputs: &[OutPin],
        ui: &mut Ui,
        scale: f32,
        snarl: &mut Snarl<T>,
    ) {
        let _ = (node, inputs, outputs, ui, scale, snarl);
    }

    /// Renders the node's footer.
    fn show_footer(
        &mut self,
        node: NodeId,
        inputs: &[InPin],
        outputs: &[OutPin],
        ui: &mut Ui,
        scale: f32,
        snarl: &mut Snarl<T>,
    ) {
        let _ = (node, inputs, outputs, ui, scale, snarl);
    }

    /// Renders the node's on-hover popup.
    fn show_on_hover_popup(
        &mut self,
        node: NodeId,
        inputs: &[InPin],
        outputs: &[OutPin],
        ui: &mut Ui,
        scale: f32,
        snarl: &mut Snarl<T>,
    ) {
        let _ = (node, inputs, outputs, ui, scale, snarl);
    }

    /// Returns color of the node's input pin.
    /// Called when pin in not visible.
    fn input_color(&mut self, pin: &InPin, style: &Style, snarl: &mut Snarl<T>) -> Color32;

    /// Returns color of the node's output pin.
    /// Called when pin in not visible.
    fn output_color(&mut self, pin: &OutPin, style: &Style, snarl: &mut Snarl<T>) -> Color32;

    /// Show context menu for the snarl.
    ///
    /// This can be used to implement menu for adding new nodes.
    fn graph_menu(&mut self, pos: Pos2, ui: &mut Ui, scale: f32, snarl: &mut Snarl<T>) {
        let _ = (pos, ui, scale, snarl);
    }

    /// Show context menu for the snarl.
    ///
    /// This can be used to implement menu for adding new nodes.
    fn node_menu(
        &mut self,
        node: NodeId,
        inputs: &[InPin],
        outputs: &[OutPin],
        ui: &mut Ui,
        scale: f32,
        snarl: &mut Snarl<T>,
    ) {
        let _ = (node, inputs, outputs, ui, scale, snarl);
    }

    /// Asks the viewer to connect two pins.
    ///
    /// This is usually happens when user drags a wire from one node's output pin to another node's input pin or vice versa.
    /// By default this method connects the pins and returns `Ok(())`.
    #[inline]
    fn connect(&mut self, from: &OutPin, to: &InPin, snarl: &mut Snarl<T>) {
        snarl.connect(from.id, to.id);
    }

    /// Asks the viewer to disconnect two pins.
    #[inline]
    fn disconnect(&mut self, from: &OutPin, to: &InPin, snarl: &mut Snarl<T>) {
        snarl.disconnect(from.id, to.id);
    }

    /// Asks the viewer to disconnect all wires from the output pin.
    ///
    /// This is usually happens when right-clicking on an output pin.
    /// By default this method disconnects the pins and returns `Ok(())`.
    #[inline]
    fn drop_outputs(&mut self, pin: &OutPin, snarl: &mut Snarl<T>) {
        snarl.drop_outputs(pin.id);
    }

    /// Asks the viewer to disconnect all wires from the input pin.
    ///
    /// This is usually happens when right-clicking on an input pin.
    /// By default this method disconnects the pins and returns `Ok(())`.
    #[inline]
    fn drop_inputs(&mut self, pin: &InPin, snarl: &mut Snarl<T>) {
        snarl.drop_inputs(pin.id);
    }
}
