use egui::{epaint::PathShape, vec2, Color32, Painter, Pos2, Rect, Shape, Stroke, Vec2};

use crate::{InPinId, OutPinId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AnyPin {
    Out(OutPinId),
    In(InPinId),
}

tiny_fn::tiny_fn! {
    /// Custom pin shape drawing function with signature
    /// `Fn(painter: &Painter, rect: Rect, fill: Color32, stroke: Stroke)`
    pub struct CustomPinShape = Fn(painter: &Painter, rect: Rect, fill: Color32, stroke: Stroke);
}

/// Shape of a pin.
pub enum PinShape {
    /// Circle shape.
    Circle,

    /// Triangle shape.
    Triangle,

    /// Square shape.
    Square,

    /// Custom shape.
    Custom(CustomPinShape<'static>),
}

/// Location of a pin.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PinLocation {
    Horizontal,
    Vertical,
}

impl PinLocation {
    /// Normalised vector pointing outwards from the input.
    pub fn input_vec(&self) -> Vec2 {
        match *self {
            PinLocation::Horizontal => vec2(-1.0, 0.0),
            PinLocation::Vertical => vec2(0.0, -1.0)
        }
    }
    /// Normalised vector pointing outwards from the output.
    pub fn output_vec(&self) -> Vec2 {
        match *self {
            PinLocation::Horizontal => vec2(1.0, 0.0),
            PinLocation::Vertical => vec2(0.0, 1.0)
        }
    }
}

/// Information about a pin returned by `SnarlViewer::show_input` and `SnarlViewer::show_output`.
pub struct PinInfo {
    /// Shape of the pin.
    pub shape: PinShape,

    /// Size of the pin.
    pub size: f32,

    /// Fill color of the pin.
    pub fill: Color32,

    /// Outline stroke of the pin.
    pub stroke: Stroke,

    /// Location of the pin.
    pub location: PinLocation,
}

impl Default for PinInfo {
    fn default() -> Self {
        PinInfo {
            shape: PinShape::Circle,
            size: 1.0,
            fill: Color32::GRAY,
            stroke: Stroke::new(1.0, Color32::BLACK),
            location: PinLocation::Horizontal,
        }
    }
}

impl PinInfo {
    /// Sets the shape of the pin.
    pub fn with_shape(mut self, shape: PinShape) -> Self {
        self.shape = shape;
        self
    }

    /// Sets the size of the pin.
    pub fn with_size(mut self, size: f32) -> Self {
        self.size = size;
        self
    }

    /// Sets the fill color of the pin.
    pub fn with_fill(mut self, fill: Color32) -> Self {
        self.fill = fill;
        self
    }

    /// Sets the outline stroke of the pin.
    pub fn with_stroke(mut self, stroke: Stroke) -> Self {
        self.stroke = stroke;
        self
    }

    /// Sets the location of the pin.
    pub fn with_location(mut self, location: PinLocation) -> Self {
        self.location = location;
        self
    }

    /// Creates a circle pin.
    pub fn circle() -> Self {
        PinInfo {
            shape: PinShape::Circle,
            ..Default::default()
        }
    }

    /// Creates a triangle pin.
    pub fn triangle() -> Self {
        PinInfo {
            shape: PinShape::Triangle,
            ..Default::default()
        }
    }

    /// Creates a square pin.
    pub fn square() -> Self {
        PinInfo {
            shape: PinShape::Square,
            ..Default::default()
        }
    }

    /// Makes the pin vertical
    pub fn vertical(mut self) -> Self {
        self.location = PinLocation::Vertical;
        self
    }

    /// Creates a square pin.
    pub fn custom<F>(f: F) -> Self
    where
        F: Fn(&Painter, Rect, Color32, Stroke) + 'static,
    {
        PinInfo {
            shape: PinShape::Custom(CustomPinShape::new(f)),
            ..Default::default()
        }
    }
}

pub fn draw_pin(painter: &Painter, pin: PinInfo, pos: Pos2, base_size: f32) {
    let size = base_size * pin.size;
    match pin.shape {
        PinShape::Circle => {
            painter.circle(pos, size * 2.0 / std::f32::consts::PI, pin.fill, pin.stroke);
        }
        PinShape::Triangle => {
            const A: Vec2 = vec2(-0.649_519, 0.4875);
            const B: Vec2 = vec2(0.649_519, 0.4875);
            const C: Vec2 = vec2(0.0, -0.6375);

            let points = vec![pos + A * size, pos + B * size, pos + C * size];

            painter.add(Shape::Path(PathShape {
                points,
                closed: true,
                fill: pin.fill,
                stroke: pin.stroke,
            }));
        }
        PinShape::Square => {
            let points = vec![
                pos + vec2(-0.5, -0.5) * size,
                pos + vec2(0.5, -0.5) * size,
                pos + vec2(0.5, 0.5) * size,
                pos + vec2(-0.5, 0.5) * size,
            ];

            painter.add(Shape::Path(PathShape {
                points,
                closed: true,
                fill: pin.fill,
                stroke: pin.stroke,
            }));
        }
        PinShape::Custom(f) => f.call(
            painter,
            Rect::from_center_size(pos, vec2(size, size)),
            pin.fill,
            pin.stroke,
        ),
    }
}
