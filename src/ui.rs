//! This module provides functionality for showing [`Snarl`] graph in [`Ui`].

use std::{collections::HashMap, hash::Hash};

use egui::{
    collapsing_header::paint_default_icon, epaint::Shadow, epaint::RectShape, pos2, vec2, Align, Color32, Frame, Id,
    Layout, Modifiers, PointerButton, Pos2, Rect, Sense, Shape, Stroke, Style, Ui, Vec2, TextureId, Rounding,
};

use crate::{InPin, InPinId, Node, NodeId, OutPin, OutPinId, Snarl};

mod background_pattern;
mod pin;
mod state;
mod viewer;
mod wire;
mod zoom;

use self::{
    pin::{draw_pin, AnyPin, PinLocation},
    state::{NewWires, NodeState, SnarlState},
    wire::{draw_wire, hit_wire, mix_colors},
    zoom::Zoom,
};

pub use self::{
    background_pattern::{BackgroundPattern, CustomBackground, Grid, Viewport},
    pin::{CustomPinShape, PinInfo, PinShape},
    viewer::SnarlViewer,
    wire::WireLayer,
};

/// Style for rendering Snarl.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "egui-probe", derive(egui_probe::EguiProbe))]
pub struct SnarlStyle {
    /// Size of pins.
    #[cfg_attr(feature = "egui-probe", egui_probe(range = 0.0..))]
    pub pin_size: Option<f32>,

    /// Width of wires.
    #[cfg_attr(feature = "egui-probe", egui_probe(range = 0.0..))]
    pub wire_width: Option<f32>,

    /// Size of wire frame which controls curvature of wires.
    pub wire_frame_size: Option<f32>,

    /// Whether to downscale wire frame when nodes are close.
    pub downscale_wire_frame: bool,

    /// Wether to upscale wire frame when nodes are close.
    pub upscale_wire_frame: bool,

    /// Whether to use simple wires without curl.
    pub simple_wire: bool,

    /// Layer where wires are rendered.
    pub wire_layer: WireLayer,

    /// Additional blank space for dragging node by header.
    pub header_drag_space: Option<Vec2>,

    /// Whether nodes can be collapsed.
    /// If true, headers will have collapsing button.
    /// When collapsed, node will not show its pins, body and footer.
    pub collapsible: bool,

    /// Background fill color.
    /// Defaults to `ui.visuals().widgets.noninteractive.bg_fill`.
    pub bg_fill: Option<Color32>,

    /// Background pattern.
    /// Defaults to [`BackgroundPattern::Grid`].
    pub bg_pattern: BackgroundPattern,

    /// Stroke for background pattern.
    /// Defaults to `ui.visuals().widgets.noninteractive.bg_stroke`.
    pub background_pattern_stroke: Option<Stroke>,

    /// Minimum scale that can be set.
    #[cfg_attr(feature = "egui-probe", egui_probe(range = 0.0..=1.0))]
    pub min_scale: f32,

    /// Maximum scale that can be set.
    #[cfg_attr(feature = "egui-probe", egui_probe(range = 1.0..))]
    pub max_scale: f32,

    /// Scale velocity when scaling with mouse wheel.
    #[cfg_attr(feature = "egui-probe", egui_probe(range = 0.0..))]
    pub scale_velocity: f32,

    /// Frame used to draw nodes.
    /// Defaults to [`Frame::window`] constructed from current ui's style.
    #[cfg_attr(feature = "serde", serde(with = "serde_frame_option"))]
    pub node_frame: Option<Frame>,

    /// Frame used to draw node headers.
    /// Defaults to [`Self::node_frame`] without shadow and transparent fill.
    ///
    /// If set, it should not have shadow and fill should be either opaque of fully transparent
    /// unless layering of header fill color with node fill color is desired.
    #[cfg_attr(feature = "serde", serde(with = "serde_frame_option"))]
    pub header_frame: Option<Frame>,

    #[doc(hidden)]
    #[cfg_attr(feature = "egui-probe", egui_probe(skip))]
    /// Do not access other than with .., here to emulate `#[non_exhaustive(pub)]`
    pub _non_exhaustive: (),
}

#[cfg(feature = "serde")]
mod serde_frame_option {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    #[derive(Serialize, Deserialize)]
    pub struct Frame {
        pub inner_margin: egui::Margin,
        pub outer_margin: egui::Margin,
        pub rounding: egui::Rounding,
        pub shadow: egui::epaint::Shadow,
        pub fill: egui::Color32,
        pub stroke: egui::Stroke,
    }

    pub fn serialize<S>(frame: &Option<egui::Frame>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match frame {
            Some(frame) => Frame {
                inner_margin: frame.inner_margin,
                outer_margin: frame.outer_margin,
                rounding: frame.rounding,
                shadow: frame.shadow,
                fill: frame.fill,
                stroke: frame.stroke,
            }
            .serialize(serializer),
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<egui::Frame>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let frame_opt = Option::<Frame>::deserialize(deserializer)?;
        Ok(frame_opt.map(|frame| egui::Frame {
            inner_margin: frame.inner_margin,
            outer_margin: frame.outer_margin,
            rounding: frame.rounding,
            shadow: frame.shadow,
            fill: frame.fill,
            stroke: frame.stroke,
        }))
    }
}

impl SnarlStyle {
    /// Creates new [`SnarlStyle`] filled with default values.
    #[must_use]
    pub const fn new() -> Self {
        SnarlStyle {
            pin_size: None,
            wire_width: None,
            wire_frame_size: None,
            downscale_wire_frame: false,
            upscale_wire_frame: true,
            simple_wire: false,
            wire_layer: WireLayer::BehindNodes,
            header_drag_space: None,
            collapsible: true,

            bg_fill: None,
            bg_pattern: background_pattern::BackgroundPattern::new(),
            background_pattern_stroke: None,

            min_scale: 0.1,
            max_scale: 2.0,
            scale_velocity: 0.005,
            node_frame: None,
            header_frame: None,

            _non_exhaustive: (),
        }
    }
}

impl Default for SnarlStyle {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

struct Input {
    hover_pos: Option<Pos2>,
    scroll_delta: f32,
    // primary_pressed: bool,
    secondary_pressed: bool,
    modifiers: Modifiers,
}

struct DrawNodeResponse {
    node_moved: Option<(NodeId, Vec2)>,
    node_to_top: Option<NodeId>,
    drag_released: bool,
    pin_hovered: Option<AnyPin>,
    rect: Rect,
}

impl<T: std::fmt::Debug> Snarl<T> {
    fn draw_background(style: &SnarlStyle, snarl_state: &SnarlState, viewport: &Rect, ui: &mut Ui) {
        let viewport = Viewport {
            rect: *viewport,
            scale: snarl_state.scale(),
            offset: snarl_state.offset(),
        };

        style.bg_pattern.draw(style, &viewport, ui);
    }

    /// Render [`Snarl`] using given viewer and style into the [`Ui`].

    pub fn show<V>(&mut self, viewer: &mut V, style: &SnarlStyle, id_source: impl Hash, ui: &mut Ui)
    where
        V: SnarlViewer<T>,
    {
        #![allow(clippy::too_many_lines)]

        let mut node_moved = None;
        let mut node_to_top = None;

        let snarl_id = ui.make_persistent_id(id_source);

        // Draw background pattern.
        let bg_fill = style
            .bg_fill
            .unwrap_or_else(|| ui.visuals().widgets.noninteractive.bg_fill);

        let bg_stroke = style
            .background_pattern_stroke
            .unwrap_or_else(|| ui.visuals().widgets.noninteractive.bg_stroke);

        let input = ui.ctx().input(|i| Input {
            scroll_delta: i.raw_scroll_delta.y,
            hover_pos: i.pointer.hover_pos(),
            modifiers: i.modifiers,
            // primary_pressed: i.pointer.primary_pressed(),
            secondary_pressed: i.pointer.secondary_pressed(),
        });

        Frame::none()
            .fill(bg_fill)
            .stroke(bg_stroke)
            .show(ui, |ui| {
                let mut bg_r = ui.allocate_rect(ui.max_rect(), Sense::click_and_drag());
                let viewport = bg_r.rect;
                ui.set_clip_rect(viewport);

                let pivot = input.hover_pos.unwrap_or_else(|| viewport.center());

                let mut snarl_state =
                    SnarlState::load(ui.ctx(), snarl_id, pivot, viewport, self, style);

                let mut node_style: Style = (**ui.style()).clone();
                node_style.zoom(snarl_state.scale());

                //Draw background
                Self::draw_background(style, &snarl_state, &viewport, ui);

                let pin_size = style
                    .pin_size
                    .zoomed(snarl_state.scale())
                    .unwrap_or(node_style.spacing.interact_size.y * 0.5);

                let wire_frame_size = style
                    .wire_frame_size
                    .zoomed(snarl_state.scale())
                    .unwrap_or(pin_size * 5.0);
                let wire_width = style
                    .wire_width
                    .zoomed(snarl_state.scale())
                    .unwrap_or(pin_size * 0.2);

                let node_frame = style
                    .node_frame
                    .zoomed(snarl_state.scale())
                    .unwrap_or_else(|| Frame::window(&node_style));

                let header_frame = style
                    .header_frame
                    .zoomed(snarl_state.scale())
                    .unwrap_or_else(|| node_frame.shadow(Shadow::NONE).fill(Color32::TRANSPARENT));

                let wire_shape_idx = match style.wire_layer {
                    WireLayer::BehindNodes => Some(ui.painter().add(Shape::Noop)),
                    WireLayer::AboveNodes => None,
                };

                // Zooming
                match input.hover_pos {
                    Some(hover_pos) if viewport.contains(hover_pos) => {
                        if input.scroll_delta != 0.0 {
                            let new_scale = (snarl_state.scale()
                                * (1.0 + input.scroll_delta * style.scale_velocity))
                                .clamp(style.min_scale, style.max_scale);

                            snarl_state.set_scale(new_scale);
                        }
                    }
                    _ => {}
                }

                let mut input_info = HashMap::new();
                let mut output_info = HashMap::new();
                let mut node_rects = Vec::new();

                let mut pin_hovered = None;

                let draw_order = self.draw_order.clone();
                let mut drag_released = false;

                for node_idx in draw_order {
                    // show_node(node_idx);
                    let response = self.draw_node(
                        ui,
                        node_idx,
                        viewer,
                        &mut snarl_state,
                        style,
                        snarl_id,
                        &node_style,
                        &node_frame,
                        &header_frame,
                        &mut input_info,
                        &input,
                        &mut output_info,
                    );
                    node_rects.push((node_idx, response.rect));

                    if let Some(v) = response.node_to_top {
                        node_to_top = Some(v);
                    }
                    if let Some(v) = response.node_moved {
                        node_moved = Some(v);
                    }
                    if let Some(v) = response.pin_hovered {
                        pin_hovered = Some(v);
                    }
                    drag_released |= response.drag_released;
                }

                // Check if any of the nodes are inside the selection box.
                if let Some(selection) = snarl_state.selection() {
                    // println!("Selection: {:?}", snarl_state.selection());
                    let in_selection_box = node_rects.iter().filter_map(|(idx, node_rect)|{
                        if node_rect.intersects(selection) {
                            Some(*idx)
                        } else {
                            None
                        }
                    }).collect::<Vec<_>>();
                    viewer.selection_pending(&in_selection_box, &input.modifiers, self);
                }

                let mut hovered_wire = None;
                let mut hovered_wire_disconnect = false;
                let mut wire_shapes = Vec::new();
                let mut wire_hit = false;

                for wire in self.wires.iter() {
                    let (from, color_from, direction_from) = if let Some((from, color_from, direction_from)) = output_info.get(&wire.out_pin) {
                        (*from, *color_from, *direction_from)
                    } else {
                        // Probably have a wire to an pin that doesn't exist.
                        // Print the node it sits in between in the panic.
                        let from = &self[wire.out_pin.node];
                        let to = &self[wire.in_pin.node];
                        panic!("could not get outpin: {wire:?}, from: {from:?} to {to:?}");
                    };
                    // let (from, color_from, direction_from) = output_info[&wire.out_pin];
                    let (to, color_to, direction_to) = input_info[&wire.in_pin];

                    if !wire_hit
                        && !snarl_state.has_new_wires()
                        && bg_r.hovered()
                        && !bg_r.dragged()
                    {
                        // Try to find hovered wire
                        // If not draggin new wire
                        // And not hovering over item above.

                        if let Some(hover_pos) = input.hover_pos {
                            wire_hit = hit_wire(
                                hover_pos,
                                wire_frame_size,
                                style.upscale_wire_frame,
                                style.downscale_wire_frame,
                                style.simple_wire,
                                (from, direction_from.output_vec()),
                                (to, direction_to.input_vec()),
                                wire_width.max(1.5),
                            );

                            if wire_hit {
                                hovered_wire = Some(wire);

                                //Remove hovered wire by second click
                                hovered_wire_disconnect |=
                                    bg_r.clicked_by(PointerButton::Secondary);

                                // Background is not hovered then.
                                bg_r.hovered = false;
                                bg_r.clicked = [false; egui::NUM_POINTER_BUTTONS];
                                bg_r.double_clicked = [false; egui::NUM_POINTER_BUTTONS];
                                bg_r.triple_clicked = [false; egui::NUM_POINTER_BUTTONS];
                            }
                        }
                    }

                    let color = mix_colors(color_from, color_to);

                    let mut draw_width = wire_width;
                    if hovered_wire == Some(wire) {
                        draw_width *= 1.5;
                    }

                    draw_wire(
                        ui,
                        &mut wire_shapes,
                        wire_frame_size,
                        style.upscale_wire_frame,
                        style.downscale_wire_frame,
                        style.simple_wire,
                        (from, direction_from.output_vec()),
                        (to, direction_to.input_vec()),
                        Stroke::new(draw_width, color),
                    );
                }

                //Remove hovered wire by second click
                if hovered_wire_disconnect {
                    if let Some(wire) = hovered_wire {
                        let out_pin = OutPin::new(self, wire.out_pin);
                        let in_pin = InPin::new(self, wire.in_pin);
                        viewer.disconnect(&out_pin, &in_pin, self);
                    }
                }

                if !input.modifiers.shift {
                    // Shift not held, always cancel selection.
                    snarl_state.selection_cancel();
                    // Shift not held, normal drag
                    if bg_r.dragged_by(PointerButton::Primary) {
                        snarl_state.pan(-bg_r.drag_delta());
                    }
                } else {
                    // Shift held, do a selection.
                    if bg_r.drag_started() {
                        if let Some(pos) = bg_r.interact_pointer_pos() {
                            snarl_state.selection_start(pos);
                        }
                    }
                    if bg_r.drag_released() { // drag_stopped() in 0.27.2
                        snarl_state.selection_cancel();
                    }
                    if bg_r.dragged_by(PointerButton::Primary) {
                        if let Some(new_pos) = bg_r.interact_pointer_pos(){
                            snarl_state.selection_drag(new_pos);
                        }
                    }
                }
                bg_r.context_menu(|ui| {
                    viewer.graph_menu(
                        snarl_state.screen_pos_to_graph(ui.cursor().min, viewport),
                        ui,
                        snarl_state.scale(),
                        self,
                    );
                });

                match snarl_state.new_wires() {
                    None => {}
                    Some(NewWires::In(pins)) => {
                        for pin in pins {
                            let from = input.hover_pos.unwrap_or(Pos2::ZERO);
                            let (to, color, direction_to) = input_info[pin];

                            draw_wire(
                                ui,
                                &mut wire_shapes,
                                wire_frame_size,
                                style.upscale_wire_frame,
                                style.downscale_wire_frame,
                                style.simple_wire,
                                (from, vec2(0.0, 0.0)),
                                (to, direction_to.input_vec()),
                                Stroke::new(wire_width, color),
                            );
                        }
                    }
                    Some(NewWires::Out(pins)) => {
                        for pin in pins {
                            let (from, color, direction_from) = output_info[pin];
                            let to = input.hover_pos.unwrap_or(Pos2::ZERO);

                            draw_wire(
                                ui,
                                &mut wire_shapes,
                                wire_frame_size,
                                style.upscale_wire_frame,
                                style.downscale_wire_frame,
                                style.simple_wire,
                                (from, direction_from.output_vec()),
                                (to, vec2(0.0, 0.0)),
                                Stroke::new(wire_width, color),
                            );
                        }
                    }
                }

                if let Some(selection) = snarl_state.selection() {
                    // Draw the rectangle.
                    ui.painter().add(Shape::Rect(RectShape{
                        rect:selection,
                        rounding: Rounding::ZERO,
                        fill: Color32::TRANSPARENT,
                        fill_texture_id: TextureId::Managed(0),
                        uv: Rect::ZERO,
                        stroke: Stroke::new(1.0, Color32::from_rgb(0x00, 0xb0, 0xb0)),
                    }));
                }

                match wire_shape_idx {
                    None => {
                        ui.painter().add(Shape::Vec(wire_shapes));
                    }
                    Some(idx) => {
                        ui.painter().set(idx, Shape::Vec(wire_shapes));
                    }
                }

                if drag_released {
                    // Always cancel the selection.
                    snarl_state.selection_cancel();

                    // Handle wires
                    let new_wires = snarl_state.take_wires();
                    if new_wires.is_some() {
                        ui.ctx().request_repaint();
                    }
                    match (new_wires, pin_hovered) {
                        (Some(NewWires::In(in_pins)), Some(AnyPin::Out(out_pin))) => {
                            for in_pin in in_pins {
                                viewer.connect(
                                    &OutPin::new(self, out_pin),
                                    &InPin::new(self, in_pin),
                                    self,
                                );
                            }
                        }
                        (Some(NewWires::Out(out_pins)), Some(AnyPin::In(in_pin))) => {
                            for out_pin in out_pins {
                                viewer.connect(
                                    &OutPin::new(self, out_pin),
                                    &InPin::new(self, in_pin),
                                    self,
                                );
                            }
                        }
                        // If going from output to output (or input to input), we can do moves and swaps;
                        // Simple one to one swaps are simple... but moving a block of outputs
                        // by holding shift is better, it also makes this logic complex :(
                        // Lets follow the following logic:
                        //  The last pin on left is called the anchor. It is the last selected pin.
                        //  The anchor moves to the right out pin, this is the most intuitive.
                        //  With this logic, we end up with two sets of pins:
                        //    - The source pins from left_out_pins, with pin indices relative to the anchor.
                        //    - The destination pins from right_output, pin indices relative to anchor, offset to right out pin.
                        //  If the source and destination pins are disjoint sets, we swap the pins.
                        //  If the source and destination pins overlap, we only allow the operation iff the destination pins
                        //   are empty after the source pins would be disconnected. This avoids having to think about where to move
                        //   that pin and potentially make a wrong choice, and it also doesn't make assumptions about the 
                        //   network allowing multiple outputs.
                        (Some(NewWires::Out(left_pins)), Some(AnyPin::Out(right_out_pin))) => {
                            let mut valid = true;
                            let left_anchor = left_pins.last().unwrap();
                            let relative_indices = left_pins.iter().map(|p| {p.output as i64 - left_anchor.output as i64}).collect::<Vec<_>>();
                            let mut right_pins = vec![];
                            for offset in relative_indices.iter() {
                                let right_value = offset + right_out_pin.output as i64;
                                if right_value < 0 {
                                    valid = false
                                }
                                right_pins.push(OutPinId{node: right_out_pin.node, output: right_value as usize});
                            }
                            // Okay, we now have the left and right sets, check if they intersect.
                            let left_set = left_pins.iter().copied().collect::<std::collections::HashSet<OutPinId>>();
                            let right_set = right_pins.iter().copied().collect::<std::collections::HashSet<OutPinId>>();

                            let swapping = left_set.is_disjoint(&right_set);
                            // There's overlap, check if the slots we'd occupy are empty after we disconnect.
                            // We will disconnect things in the left set, so we only need to check what is in right
                            // and not in left.
                            let overlap_valid = swapping || right_set.difference(&left_set).all(|v|self.out_pin(*v).remotes.is_empty());
                            if valid && overlap_valid {
                                let mut to_disconnect = vec![];
                                let mut to_connect = vec![];
                                for (left_out, right_out) in left_pins.iter().zip(right_pins.iter()) {
                                    let left_out_pin = self.out_pin(*left_out);
                                    for input in left_out_pin.remotes.iter() {
                                        to_disconnect.push((*left_out, *input));
                                        to_connect.push((*right_out, *input));
                                    }
            
                                    if swapping {
                                        let right_out_pin = self.out_pin(*right_out);
                                        for input in right_out_pin.remotes.iter() {
                                            to_disconnect.push((*right_out, *input));
                                            to_connect.push((*left_out, *input));
                                        }
                                    }
                                }

                                // Disconnnect then reconnect, that should always be the valid order.
                                for (disconnect_out, disconnect_in) in to_disconnect {
                                    viewer.disconnect(&OutPin::new(self, disconnect_out), &InPin::new(self, disconnect_in), self);
                                }
                                for (connect_out, connect_in) in to_connect {
                                    viewer.connect(&OutPin::new(self, connect_out), &InPin::new(self, connect_in), self);
                                }
                            }
                        }
                        // If going from input to input, swap all connections.
                        (Some(NewWires::In(left_pins)), Some(AnyPin::In(right_pin))) => {
                            let mut valid = true;
                            let left_anchor = left_pins.last().unwrap();
                            let relative_indices = left_pins.iter().map(|p| {p.input as i64 - left_anchor.input as i64}).collect::<Vec<_>>();
                            let mut right_pins = vec![];
                            for offset in relative_indices.iter() {
                                let right_value = offset + right_pin.input as i64;
                                if right_value < 0 {
                                    valid = false
                                }
                                right_pins.push(InPinId{node: right_pin.node, input: right_value as usize});
                            }
                            // Okay, we now have the left and right sets, check if they intersect.
                            let left_set = left_pins.iter().copied().collect::<std::collections::HashSet<InPinId>>();
                            let right_set = right_pins.iter().copied().collect::<std::collections::HashSet<InPinId>>();

                            let swapping = left_set.is_disjoint(&right_set);
                            // There's overlap, check if the slots we'd occupy are empty after we disconnect.
                            // We will disconnect things in the left set, so we only need to check what is in right
                            // and not in left.
                            let overlap_valid = swapping || right_set.difference(&left_set).all(|v|self.in_pin(*v).remotes.is_empty());
                            if valid && overlap_valid {
                                let mut to_disconnect = vec![];
                                let mut to_connect = vec![];
                                for (left_in, right_in) in left_pins.iter().zip(right_pins.iter()) {
                                    let left_in_pin = self.in_pin(*left_in);
                                    for output in left_in_pin.remotes.iter() {
                                        to_disconnect.push((*output, *left_in));
                                        to_connect.push((*output, *right_in));
                                    }
            
                                    if swapping {
                                        let right_out_pin = self.in_pin(*right_in);
                                        for output in right_out_pin.remotes.iter() {
                                            to_disconnect.push((*output, *right_in));
                                            to_connect.push((*output, *left_in));
                                        }
                                    }
                                }

                                // Disconnnect then reconnect, that should always be the valid order.
                                for (disconnect_out, disconnect_in) in to_disconnect {
                                    viewer.disconnect(&OutPin::new(self, disconnect_out), &InPin::new(self, disconnect_in), self);
                                }
                                for (connect_out, connect_in) in to_connect {
                                    viewer.connect(&OutPin::new(self, connect_out), &InPin::new(self, connect_in), self);
                                }
                            }
                        }

                        _ => {}
                    }
                }

                ui.advance_cursor_after_rect(Rect::from_min_size(viewport.min, Vec2::ZERO));

                snarl_state.store(ui.ctx());
            });

        if let Some((node, delta)) = node_moved {
            ui.ctx().request_repaint();
            let node = &mut self.nodes[node.0];
            node.pos += delta;
        }

        if let Some(node_idx) = node_to_top {
            ui.ctx().request_repaint();
            if let Some(order) = self.draw_order.iter().position(|idx| *idx == node_idx) {
                self.draw_order.remove(order);
                self.draw_order.push(node_idx);
            }
        }
    }

    //First step for split big function to parts
    /// Draw one node. Return Pins info
    #[inline]
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::too_many_arguments)]
    fn draw_node<V>(
        &mut self,
        ui: &mut Ui,
        node: NodeId,
        viewer: &mut V,
        snarl_state: &mut SnarlState,
        style: &SnarlStyle,
        snarl_id: Id,
        node_style: &Style,
        node_frame: &Frame,
        header_frame: &Frame,
        input_positions: &mut HashMap<InPinId, (Pos2, Color32, PinLocation)>,
        input: &Input,
        output_positions: &mut HashMap<OutPinId, (Pos2, Color32, PinLocation)>,
    ) -> DrawNodeResponse
    where
        V: SnarlViewer<T>,
    {
        // Copy the node frame such that we can modify its stroke and fill.
        let mut node_frame = node_frame.clone();
        let mut header_frame = header_frame.clone();

        let Node {
            pos,
            open,
            ref value,
        } = self.nodes[node.0];

        let mut response = DrawNodeResponse {
            node_to_top: None,
            node_moved: None,
            drag_released: false,
            pin_hovered: None,
            rect: Rect::NOTHING,
        };

        let viewport = ui.max_rect();

        // Collect pins
        let inputs_count = viewer.inputs(value);
        let outputs_count = viewer.outputs(value);

        let node_pos = snarl_state.graph_pos_to_screen(pos, viewport);

        // Generate persistent id for the node.
        let node_id = snarl_id.with(("snarl-node", node));

        let openness = ui.ctx().animate_bool(node_id, open);

        let mut node_state =
            NodeState::load(ui.ctx(), node_id, &node_style.spacing, snarl_state.scale());

        let node_rect = node_state.node_rect(node_pos, openness);
        response.rect = node_rect;

        // Rect for node + frame margin.
        let node_frame_rect = node_frame.total_margin().expand_rect(node_rect);

        let pin_size = style
            .pin_size
            .zoomed(snarl_state.scale())
            .unwrap_or(node_style.spacing.interact_size.y * 0.5);

        let header_drag_space = style
            .header_drag_space
            .zoomed(snarl_state.scale())
            .unwrap_or_else(|| vec2(node_style.spacing.icon_width, node_style.spacing.icon_width));

        let inputs = (0..inputs_count)
            .map(|idx| InPin::new(self, InPinId { node, input: idx }))
            .collect::<Vec<_>>();

        let outputs = (0..outputs_count)
            .map(|idx| OutPin::new(self, OutPinId { node, output: idx }))
            .collect::<Vec<_>>();

        // Interact with node frame.
        let r = ui.interact(node_frame_rect, node_id, Sense::click_and_drag());

        if r.dragged_by(PointerButton::Primary) {
            response.node_moved = Some((node, snarl_state.screen_vec_to_graph(r.drag_delta())));
        }
        if r.clicked() || r.dragged() {
            response.node_to_top = Some(node);
        }
        r.context_menu(|ui| {
            viewer.node_menu(node, &inputs, &outputs, ui, snarl_state.scale(), self);
        });

        // We could also do something here where we check the pointer position.
        let draw_wiring_pins = snarl_state.has_new_wires();

        if !self.nodes.contains(node.0) {
            node_state.clear(ui.ctx());
            // If removed
            return response;
        }

        if viewer.has_on_hover_popup(&self.nodes[node.0].value) {
            r.on_hover_ui_at_pointer(|ui| {
                viewer.show_on_hover_popup(node, &inputs, &outputs, ui, snarl_state.scale(), self);
            });
        }

        if !self.nodes.contains(node.0) {
            node_state.clear(ui.ctx());
            // If removed
            return response;
        }

        let node_ui = &mut ui.child_ui_with_id_source(
            node_frame_rect,
            Layout::top_down(Align::Center),
            ("node", node_id),
        );
        node_ui.set_style(node_style.clone());

        // Override the frame strokes, if set.
        if let Some(stroke_override) = viewer.node_stroke(node, &node_frame.stroke, self) {
            node_frame.stroke = stroke_override;
            header_frame.stroke = stroke_override;
        }

        if let Some(fill_override) = viewer.node_fill(node, &node_frame.fill, self) {
            node_frame.fill = fill_override;
            header_frame.fill = fill_override;
        }

        node_frame.show(node_ui, |ui| {
            // Render header frame.
            let mut header_rect = node_rect;

            let mut header_frame_rect = header_frame.total_margin().expand_rect(header_rect);

            // Show node's header
            let header_ui = &mut ui.child_ui_with_id_source(
                header_frame_rect,
                Layout::top_down(Align::Center),
                "header",
            );

            header_frame.show(header_ui, |ui: &mut Ui| {
                ui.with_layout(Layout::left_to_right(Align::Min), |ui| {
                    if style.collapsible {
                        let (_, r) = ui.allocate_exact_size(
                            vec2(node_style.spacing.icon_width, node_style.spacing.icon_width),
                            Sense::click(),
                        );
                        paint_default_icon(ui, openness, &r);

                        if r.clicked_by(PointerButton::Primary) {
                            // Toggle node's openness.
                            self.open_node(node, !open);
                        }
                    }

                    ui.allocate_exact_size(header_drag_space, Sense::hover());

                    viewer.show_header(node, &inputs, &outputs, ui, snarl_state.scale(), self);

                    header_rect = ui.min_rect();
                });

                header_frame_rect = header_frame.total_margin().expand_rect(header_rect);

                ui.advance_cursor_after_rect(Rect::from_min_max(
                    header_rect.min,
                    pos2(
                        f32::max(header_rect.max.x, node_rect.max.x),
                        header_rect.min.y,
                    ),
                ));
            });
            let header_rect = header_rect;
            ui.expand_to_include_rect(header_rect);
            let header_size = header_rect.size();
            node_state.set_header_height(header_size.y);

            if !self.nodes.contains(node.0) {
                // If removed
                return;
            }

            let min_pin_y = header_rect.center().y;

            let input_x = node_frame_rect.left() + node_frame.inner_margin.left + pin_size;

            let output_x = node_frame_rect.right() - node_frame.inner_margin.right - pin_size;

            // Input/output pin block

            if (openness < 1.0 && open) || (openness > 0.0 && !open) {
                ui.ctx().request_repaint();
            }

            // Pins are placed under the header and must not go outside of the header frame.
            let payload_rect = Rect::from_min_max(
                pos2(
                    header_rect.min.x,
                    header_frame_rect.max.y + node_style.spacing.item_spacing.y
                        - node_state.payload_offset(openness),
                ),
                pos2(f32::max(node_rect.max.x, header_rect.max.x), f32::INFINITY),
            );

            let payload_clip_rect = Rect::from_min_max(
                pos2(header_rect.min.x, header_frame_rect.max.y),
                pos2(f32::max(node_rect.max.x, header_rect.max.x), f32::INFINITY),
            );

            // Show input pins.

            // Top row inputs sit exactly on the header_frame top line.
            // This box is zero height, but it's only used for positioning
            // of the input pins, so maybe that's fine?
            let top_input_rect = Rect::from_min_max(
                pos2(node_frame_rect.min.x, node_frame_rect.min.y),
                pos2(node_frame_rect.max.x, node_frame_rect.min.y),
            );

            // Input pins on the top.
            let inputs_top_row_ui = &mut ui.child_ui_with_id_source(
                top_input_rect,
                Layout::left_to_right(Align::Center),
                "top_inputs",
            );
            inputs_top_row_ui.set_clip_rect(viewport);

            // Input pins on the left.
            let inputs_ui = &mut ui.child_ui_with_id_source(
                payload_rect,
                Layout::top_down(Align::Min),
                "inputs",
            );
            inputs_ui.set_clip_rect(payload_clip_rect.intersect(viewport));

            for in_pin in &inputs {
                // Depending on pin location, determine ui.
                let (draw_base, vertical_input) : (&mut Ui, bool) = if let Some(pin_info) = viewer.vertical_input(in_pin, self) {
                    if pin_info.location == pin::PinLocation::Vertical {
                        (inputs_top_row_ui, true)
                    } else {
                        (inputs_ui, false)
                    }
                } else {
                    (inputs_ui, false)
                };
                // Show input pin.
                draw_base.with_layout(Layout::left_to_right(Align::Min), |ui| {
                    // Allocate space for pin shape.
                    let (pin_id, _) = ui.allocate_space(vec2(pin_size * 1.5, pin_size * 1.5));

                    let y0 = ui.cursor().min.y;

                    // Show input content
                    let pin_info = viewer.show_input(in_pin, ui, snarl_state.scale(), self);
                    if !self.nodes.contains(node.0) {
                        // If removed
                        return;
                    }

                    let y1 = ui.min_rect().max.y;

                    // ui.end_row();

                    // Position the pin appropriately based on input location.
                    let (x, y) = if !vertical_input {
                        (input_x, min_pin_y.max((y0 + y1) * 0.5))
                    } else {
                        (ui.cursor().min.x, y0)
                    };

                    let pin_pos = pos2(x, y);

                    input_positions.insert(in_pin.id, (pin_pos, pin_info.fill, pin_info.location));

                    if !draw_wiring_pins && pin_info.visibility == pin::PinVisibility::Wiring {
                        return;
                    }

                    // Interact with pin shape.
                    let r = ui.interact(
                        Rect::from_center_size(pin_pos, vec2(pin_size, pin_size)),
                        pin_id,
                        Sense::click_and_drag(),
                    );

                    if r.clicked_by(PointerButton::Secondary) {
                        if snarl_state.has_new_wires() {
                            snarl_state.remove_new_wire_in(in_pin.id);
                        } else {
                            viewer.drop_inputs(in_pin, self);
                            if !self.nodes.contains(node.0) {
                                // If removed
                                return;
                            }
                        }
                    }
                    if r.drag_started_by(PointerButton::Primary) {
                        if input.modifiers.command {
                            snarl_state.start_new_wires_out(&in_pin.remotes);
                            if !input.modifiers.shift {
                                self.drop_inputs(in_pin.id);
                                if !self.nodes.contains(node.0) {
                                    // If removed
                                    return;
                                }
                            }
                        } else {
                            snarl_state.start_new_wire_in(in_pin.id);
                        }
                    }
                    if r.drag_released() {
                        response.drag_released = true;
                    }

                    let mut pin_size = pin_size;

                    match input.hover_pos {
                        Some(hover_pos) if r.rect.contains(hover_pos) => {
                            if input.modifiers.shift {
                                snarl_state.add_new_wire_in(in_pin.id);
                            } else if input.secondary_pressed {
                                snarl_state.remove_new_wire_in(in_pin.id);
                            }
                            response.pin_hovered = Some(AnyPin::In(in_pin.id));
                            pin_size *= 1.2;
                        }
                        _ => {}
                    }

                    draw_pin(ui.painter(), pin_info, pin_pos, pin_size);
                });
            }
            let inputs_rect = inputs_ui.min_rect();
            ui.expand_to_include_rect(inputs_rect.intersect(payload_clip_rect));

            let inputs_size = inputs_rect.size();

            if !self.nodes.contains(node.0) {
                // If removed
                return;
            }

            // Show output pins, either at the bottom or on the right.
            let bottom_output_rect = Rect::from_min_max(
                pos2(node_frame_rect.min.x, node_frame_rect.max.y),
                pos2(node_frame_rect.max.x, node_frame_rect.max.y),
            );
            let outputs_bottom_row_ui = &mut ui.child_ui_with_id_source(
                bottom_output_rect,
                Layout::left_to_right(Align::Center),
                "bottom_outputs",
            );
            outputs_bottom_row_ui.set_clip_rect(viewport);

            // Outputs are placed under the header and must not go outside of the header frame.
            let outputs_ui = &mut ui.child_ui_with_id_source(
                payload_rect,
                Layout::top_down(Align::Max),
                "outputs",
            );
            outputs_ui.set_clip_rect(payload_clip_rect.intersect(viewport));

            // Output pins on the right or bottom.
            for out_pin in &outputs {
                // Depending on pin location, determine ui.
                let (draw_base, layout, vertical_output) : (&mut Ui, Layout, bool) = if let Some(pin_info) = viewer.vertical_output(out_pin, self) {
                    if pin_info.location == pin::PinLocation::Vertical {
                        (outputs_bottom_row_ui, Layout::left_to_right(Align::Min), true)
                    } else {
                        (outputs_ui, Layout::right_to_left(Align::Min), false)
                    }
                } else {
                    (outputs_ui, Layout::right_to_left(Align::Min), false)
                };
                // Show output pin.
                draw_base.with_layout(layout, |ui| {
                    // Allocate space for pin shape.
                    let (pin_id, _) = ui.allocate_space(vec2(pin_size * 1.5, pin_size * 1.5));

                    let y0 = ui.cursor().min.y;

                    // Show output content
                    let pin_info = viewer.show_output(out_pin, ui, snarl_state.scale(), self); // TODO? Prevent calling this.
                    if !self.nodes.contains(node.0) {
                        // If removed
                        return;
                    }

                    let y1 = ui.min_rect().max.y;

                    // ui.end_row();

                    // Position the pin appropriately.
                    let (x, y) = if !vertical_output {
                        // Centered vertically.
                        (output_x, min_pin_y.max((y0 + y1) * 0.5))
                    } else {
                        (ui.cursor().min.x, y0)
                    };

                    let pin_pos = pos2(x, y);

                    output_positions.insert(out_pin.id, (pin_pos, pin_info.fill, pin_info.location));

                    if !draw_wiring_pins && pin_info.visibility == pin::PinVisibility::Wiring {
                        return;
                    }

                    let r = ui.interact(
                        Rect::from_center_size(pin_pos, vec2(pin_size, pin_size)),
                        pin_id,
                        Sense::click_and_drag(),
                    );

                    if r.clicked_by(PointerButton::Secondary) {
                        if snarl_state.has_new_wires() {
                            snarl_state.remove_new_wire_out(out_pin.id);
                        } else {
                            viewer.drop_outputs(out_pin, self);
                            if !self.nodes.contains(node.0) {
                                // If removed
                                return;
                            }
                        }
                    }
                    if r.drag_started_by(PointerButton::Primary) {
                        if input.modifiers.command {
                            snarl_state.start_new_wires_in(&out_pin.remotes);

                            if !input.modifiers.shift {
                                self.drop_outputs(out_pin.id);
                                if !self.nodes.contains(node.0) {
                                    // If removed
                                    return;
                                }
                            }
                        } else {
                            snarl_state.start_new_wire_out(out_pin.id);
                        }
                    }
                    if r.drag_released() {
                        response.drag_released = true;
                    }

                    let mut pin_size = pin_size;
                    match input.hover_pos {
                        Some(hover_pos) if r.rect.contains(hover_pos) => {
                            if input.modifiers.shift {
                                snarl_state.add_new_wire_out(out_pin.id);
                            } else if input.secondary_pressed {
                                snarl_state.remove_new_wire_out(out_pin.id);
                            }
                            response.pin_hovered = Some(AnyPin::Out(out_pin.id));
                            pin_size *= 1.2;
                        }
                        _ => {}
                    }
                    draw_pin(ui.painter(), pin_info, pin_pos, pin_size);
                });
            }
            let outputs_rect = outputs_ui.min_rect();
            ui.expand_to_include_rect(outputs_rect.intersect(payload_clip_rect));
            let outputs_size = outputs_rect.size();

            if !self.nodes.contains(node.0) {
                // If removed
                return;
            }

            let mut new_pins_size = vec2(
                inputs_size.x + outputs_size.x + node_style.spacing.item_spacing.x,
                f32::max(inputs_size.y, outputs_size.y),
            );
            new_pins_size.x = new_pins_size.x.max(inputs_top_row_ui.min_rect().width().max(outputs_bottom_row_ui.min_rect().width()));

            let mut pins_bottom = f32::max(inputs_rect.bottom(), outputs_rect.bottom());

            // Show body if there's one.
            if viewer.has_body(&self.nodes.get(node.0).unwrap().value) && open {
                let body_left = inputs_rect.right() + node_style.spacing.item_spacing.x;
                let body_right = outputs_rect.left() - node_style.spacing.item_spacing.x;
                let body_top = payload_rect.top();

                let mut body_rect =
                    Rect::from_min_max(pos2(body_left, body_top), pos2(body_right, f32::INFINITY));
                body_rect = node_state.align_body(body_rect);

                let mut body_ui = ui.child_ui_with_id_source(
                    body_rect,
                    Layout::left_to_right(Align::Min),
                    "body",
                );
                body_ui.set_clip_rect(payload_clip_rect.intersect(viewport));

                viewer.show_body(
                    node,
                    &inputs,
                    &outputs,
                    &mut body_ui,
                    snarl_state.scale(),
                    self,
                );

                body_rect = body_ui.min_rect();
                ui.expand_to_include_rect(body_rect.intersect(payload_clip_rect));
                let body_size = body_rect.size();
                node_state.set_body_width(body_size.x);

                new_pins_size.x += body_size.x + node_style.spacing.item_spacing.x;
                new_pins_size.y = f32::max(new_pins_size.y, body_size.y);

                pins_bottom = f32::max(pins_bottom, body_rect.bottom());

                if !self.nodes.contains(node.0) {
                    // If removed
                    return;
                }
            }

            if viewer.has_footer(&self.nodes[node.0].value) && open {
                let footer_left = node_rect.left();
                let footer_right = node_rect.right();
                let footer_top = pins_bottom + node_style.spacing.item_spacing.y;

                let mut footer_rect = Rect::from_min_max(
                    pos2(footer_left, footer_top),
                    pos2(footer_right, f32::INFINITY),
                );

                footer_rect = node_state.align_footer(footer_rect);

                let mut footer_ui = ui.child_ui_with_id_source(
                    footer_rect,
                    Layout::left_to_right(Align::Min),
                    "footer",
                );
                footer_ui.set_clip_rect(payload_clip_rect.intersect(viewport));

                viewer.show_footer(
                    node,
                    &inputs,
                    &outputs,
                    &mut footer_ui,
                    snarl_state.scale(),
                    self,
                );

                footer_rect = footer_ui.min_rect();
                ui.expand_to_include_rect(footer_rect.intersect(payload_clip_rect));
                let footer_size = footer_rect.size();
                node_state.set_footer_width(footer_size.x);

                new_pins_size.x = f32::max(new_pins_size.x, footer_size.x);
                new_pins_size.y += footer_size.y + node_style.spacing.item_spacing.y;

                if !self.nodes.contains(node.0) {
                    // If removed
                    return;
                }
            }

            node_state.set_size(vec2(
                f32::max(header_size.x, new_pins_size.x),
                header_size.y
                    + header_frame.total_margin().bottom
                    + node_style.spacing.item_spacing.y
                    + new_pins_size.y,
            ));
        });

        if !self.nodes.contains(node.0) {
            ui.ctx().request_repaint();
            node_state.clear(ui.ctx());
            // If removed
            return response;
        }

        node_state.store(ui.ctx());
        ui.ctx().request_repaint();
        response
    }
}
