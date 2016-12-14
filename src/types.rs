use std::collections::HashMap;
use std::str::FromStr;
use std::fmt;
use std::error::Error;

/// A mesh made up of triangles loaded from some OBJ file
///
/// It is assumed that all meshes will at least have positions, but normals and texture coordinates
/// are optional. If no normals or texture coordinates where found then the corresponding vecs for
/// the mesh will be empty. Values are stored packed as floats in vecs, eg. the positions member of
/// a loaded mesh will contain `[x, y, z, x, y, z, ...]` which you can then use however you like.
/// Indices are also loaded and may re-use vertices already existing in the mesh, this data is
/// stored in the `indices` member.
///
/// # Example:
/// Load the Cornell box and get the attributes of the first vertex. It's assumed all meshes will
/// have positions (required), but normals and texture coordinates are optional, in which case the
/// corresponding Vec will be empty.
///
/// ```
/// use std::path::Path;
///
/// let cornell_box = tobj::load_obj(&Path::new("cornell_box.obj"));
/// assert!(cornell_box.is_ok());
/// let (models, materials) = cornell_box.unwrap();
///
/// let mesh = &models[0].mesh;
/// let i = mesh.indices[0] as usize;
/// // pos = [x, y, z]
/// let pos = [mesh.positions[i * 3], mesh.positions[i * 3 + 1],
///             mesh.positions[i * 3 + 2]];
///
/// if !mesh.normals.is_empty() {
///     // normal = [x, y, z]
///     let normal = [mesh.normals[i * 3], mesh.normals[i * 3 + 1],
///                   mesh.normals[i * 3 + 2]];
/// }
///
/// if !mesh.texcoords.is_empty() {
///     // texcoord = [u, v];
///     let texcoord = [mesh.texcoords[i * 2], mesh.texcoords[i * 2 + 1]];
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Mesh {
    /// Flattened 3 component floating point vectors, storing positions of vertices in the mesh
    pub positions: Vec<f32>,
    /// Flattened 3 component floating point vectors, storing normals of vertices in the mesh. Not
    /// all meshes have normals, if no normals are specified this Vec will be empty
    pub normals: Vec<f32>,
    /// Flattened 2 component floating point vectors, storing texture coordinates of vertices in
    /// the mesh. Not all meshes have normals, if no texture coordinates are specified this Vec
    /// will be empty
    pub texcoords: Vec<f32>,
    /// Indices for vertices of each triangle. Each face in the mesh is a triangle and the indices
    /// specify the position, normal and texture coordinate for each vertex of the face.
    pub indices: Vec<u32>,
    /// Optional material name associated with this mesh. The material id indexes into the Vec of
    /// Materials loaded from the associated MTL file
    pub material: Option<String>,
}

impl Mesh {
    /// Create a new mesh specifying the geometry for the mesh
    pub fn new(pos: Vec<f32>, norm: Vec<f32>, tex: Vec<f32>, indices: Vec<u32>, material: Option<String>)
        -> Mesh {
        Mesh {
            positions: pos,
            normals: norm,
            texcoords: tex,
            indices: indices,
            material: material,
        }
    }
    /// Create a new empty mesh
    pub fn empty() -> Mesh {
        Mesh {
            positions: Vec::new(),
            normals: Vec::new(),
            texcoords: Vec::new(),
            indices: Vec::new(),
            material: None,
        }
    }
}

/// A named model within the file, associates some mesh with a name that was specified with an `o`
/// or `g` keyword in the OBJ file
#[derive(Clone, Debug)]
pub struct Model {
    /// Mesh used by the model containing its geometry
    pub mesh: Mesh,
    /// Name assigned to this mesh
    pub name: String,
}

impl Model {
    /// Create a new model, associating a name with a mesh
    pub fn new(mesh: Mesh, name: String) -> Model {
        Model { mesh: mesh, name: name }
    }
}

/// A material that may be referenced by one or more meshes. Standard MTL attributes are supported.
/// Any unrecognized parameters will be stored as key-value pairs in the `unknown_param` `HashMap`,
/// which maps the unknown parameter to the value set for it.
#[derive(Clone, Debug)]
pub struct Material {
    /// Material name as specified in the MTL file
    pub name: String,
    /// Ambient color of the material
    pub ambient: [f32; 3],
    /// Diffuse color of the material
    pub diffuse: [f32; 3],
    /// Specular color of the material
    pub specular: [f32; 3],
    /// Material shininess attribute
    pub shininess: f32,
    /// Dissolve attribute is the alpha term for the material. Referred to as dissolve since that's
    /// what the MTL file format docs refer to it as
    pub dissolve: f32,
    /// Name of the ambient texture file for the material. No path is pre-pended to the texture
    /// file names specified in the MTL file
    pub ambient_texture: String,
    /// Name of the diffuse texture file for the material. No path is pre-pended to the texture
    /// file names specified in the MTL file
    pub diffuse_texture: String,
    /// Name of the specular texture file for the material. No path is pre-pended to the texture
    /// file names specified in the MTL file
    pub specular_texture: String,
    /// Name of the normal map texture file for the material. No path is pre-pended to the texture
    /// file names specified in the MTL file
    pub normal_texture: String,
    /// Name of the alpha map texture file for the material. No path is pre-pended to the texture
    /// file names specified in the MTL file. Referred to as dissolve to match the MTL file format
    /// specification
    pub dissolve_texture: String,
    /// Key value pairs of any unrecognized parameters encountered while parsing the material
    pub unknown_param: HashMap<String, String>,
}

impl Material {
    pub fn empty() -> Material {
        Material { name: String::new(), ambient: [0.0; 3], diffuse: [0.0; 3], specular: [0.0; 3],
                   shininess: 0.0, dissolve: 1.0, ambient_texture: String::new(),
                   diffuse_texture: String::new(), specular_texture: String::new(),
                   normal_texture: String::new(), dissolve_texture: String::new(),
                   unknown_param: HashMap::new() }
    }
}

/// Possible errors that may occur while loading OBJ and MTL files
#[derive(Debug)]
pub enum LoadError {
    OpenFileFailed,
    ReadError,
    UnrecognizedCharacter,
    PositionParseError,
    NormalParseError,
    TexcoordParseError,
    FaceParseError,
    MaterialParseError,
    InvalidObjectName,
    GenericFailure,
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str(self.description())
    }
}

impl Error for LoadError {
    fn description(&self) -> &str {
        match *self {
            LoadError::OpenFileFailed =>
                "open file failed",
            LoadError::ReadError =>
                "read error",
            LoadError::UnrecognizedCharacter =>
                "unrecognized character",
            LoadError::PositionParseError =>
                "position parse error",
            LoadError::NormalParseError =>
                "normal parse error",
            LoadError::TexcoordParseError =>
                "texcoord parse error",
            LoadError::FaceParseError =>
                "face parse error",
            LoadError::MaterialParseError =>
                "material parse error",
            LoadError::InvalidObjectName =>
                "invalid object name",
            LoadError::GenericFailure =>
                "generic failure",
        }
    }
}


/// `LoadResult` is a result containing all the models loaded from the file and 
/// the paths of all the material libraries found.
pub type LoadResult = Result<(Vec<Model>, Vec<String>), LoadError>;

/// `MTLLoadResult` is a result containing all the materials loaded from the file and a map of MTL
/// name to index or the error that occured while loading
pub type MTLLoadResult = Result<HashMap<String, Material>, LoadError>;

/// Struct storing indices corresponding to the vertex
/// Some vertices may not have texcoords or normals, 0 is used to indicate this
/// as OBJ indices begin at 1
#[derive(Hash, Eq, PartialEq, PartialOrd, Ord, Debug, Copy, Clone)]
pub struct VertexIndices {
    pub v: isize,
    pub vt: isize,
    pub vn: isize,
}

impl VertexIndices {
    /// Parse the vertex indices from the face string
    /// Valid face strings are those that are valid for a Wavefront OBJ file
    /// Also handles relative face indices (negative values) which is why passing the number of
    /// positions, texcoords and normals is required
    /// Returns None if the face string is invalid
    pub fn parse(face_str: &str, pos_sz: usize, tex_sz: usize, norm_sz: usize) -> Option<VertexIndices> {
        let mut indices = [-1; 3];
        for i in face_str.split('/').enumerate() {
            // Catch case of v//vn where we'll find an empty string in one of our splits
            // since there are no texcoords for the mesh
            if !i.1.is_empty() {
                match isize::from_str(i.1) {
                    Ok(x) => {
                        // Handle relative indices
                        indices[i.0] =
                            if x < 0 {
                                match i.0 {
                                    0 => x + pos_sz as isize,
                                    1 => x + tex_sz as isize,
                                    2 => x + norm_sz as isize,
                                    _ => panic!("Invalid number of elements for a face (> 3)!"),
                                }
                            } else {
                                x - 1
                            };
                    },
                    Err(_) => return None,
                }
            }
        }
        Some(VertexIndices { v: indices[0], vt: indices[1], vn: indices[2] })
    }
}

/// Enum representing either a quad or triangle face, storing indices for the face vertices
#[derive(Debug)]
pub enum Face {
    Triangle(VertexIndices, VertexIndices, VertexIndices),
    Quad(VertexIndices, VertexIndices, VertexIndices, VertexIndices),
    Polygon(Vec<VertexIndices>),
}
