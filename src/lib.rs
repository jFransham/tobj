//! Tiny OBJ loader, inspired by Syoyo's excellent [tinyobjloader](https://github.com/syoyo/tinyobjloader).
//! Aims to be a simple and lightweight option for loading OBJ files, just returns two vecs
//! containing loaded models and materials. All models are made of triangles, any quad or polygon faces
//! in an OBJ file will be converted to triangles. Note that only polygons that are trivially
//! convertible to triangle fans are supported, arbitrary polygons may not behave as expected.
//! The best solution would be to re-export your mesh using only triangles in your modeling software.
//!
//! It is assumed that all meshes will at least have positions, but normals and texture coordinates
//! are optional. If no normals or texture coordinates were found then the corresponding vecs for
//! the mesh will be empty. Values are stored packed as floats in vecs, eg. the positions member of
//! a loaded mesh will contain `[x, y, z, x, y, z, ...]` which you can then use however you like.
//! Indices are also loaded and may re-use vertices already existing in the mesh, this data is
//! stored in the `indices` member.
//!
//! Standard MTL attributes are supported as well and any unrecognized parameters will be stored in a
//! `HashMap` containing the key-value pairs of the unrecognized parameter and its value.
//!
//! # Example
//! In this simple example we load the classic Cornell Box model that only defines positions and
//! print out its attributes. This example is a slightly trimmed down version of `print_model_info`
//! and `print_material_info` combined together, see them for a version that also prints out
//! normals and texture coordinates if the model has them.
//!
//! ```
//! use std::path::Path;
//! use tobj;
//!
//! let cornell_box = tobj::load_obj(&Path::new("cornell_box.obj"));
//! assert!(cornell_box.is_ok());
//! let (models, materials) = cornell_box.unwrap();
//!
//! println!("# of models: {}", models.len());
//! println!("# of materials: {}", materials.len());
//! for (i, m) in models.iter().enumerate() {
//! 	let mesh = &m.mesh;
//! 	println!("model[{}].name = \'{}\'", i, m.name);
//! 	println!("model[{}].mesh.material_id = {:?}", i, mesh.material_id);
//!
//! 	println!("Size of model[{}].indices: {}", i, mesh.indices.len());
//! 	for f in 0..mesh.indices.len() / 3 {
//! 		println!("    idx[{}] = {}, {}, {}.", f, mesh.indices[3 * f],
//! 			mesh.indices[3 * f + 1], mesh.indices[3 * f + 2]);
//! 	}
//!
//! 	// Normals and texture coordinates are also loaded, but not printed in this example
//! 	println!("model[{}].vertices: {}", i, mesh.positions.len() / 3);
//! 	assert!(mesh.positions.len() % 3 == 0);
//! 	for v in 0..mesh.positions.len() / 3 {
//! 		println!("    v[{}] = ({}, {}, {})", v, mesh.positions[3 * v],
//! 			mesh.positions[3 * v + 1], mesh.positions[3 * v + 2]);
//! 	}
//! }
//! for (i, m) in materials.iter().enumerate() {
//! 	println!("material[{}].name = \'{}\'", i, m.name);
//! 	println!("    material.Ka = ({}, {}, {})", m.ambient[0], m.ambient[1], m.ambient[2]);
//! 	println!("    material.Kd = ({}, {}, {})", m.diffuse[0], m.diffuse[1], m.diffuse[2]);
//! 	println!("    material.Ks = ({}, {}, {})", m.specular[0], m.specular[1], m.specular[2]);
//! 	println!("    material.Ns = {}", m.shininess);
//! 	println!("    material.d = {}", m.dissolve);
//! 	println!("    material.map_Ka = {}", m.ambient_texture);
//! 	println!("    material.map_Kd = {}", m.diffuse_texture);
//! 	println!("    material.map_Ks = {}", m.specular_texture);
//! 	println!("    material.map_Ns = {}", m.normal_texture);
//! 	println!("    material.map_d = {}", m.dissolve_texture);
//! 	for (k, v) in &m.unknown_param {
//! 		println!("    material.{} = {}", k, v);
//! 	}
//! }
//! ```
//!
//! # Rendering Examples
//! For an example of integration with [glium](https://github.com/tomaka/glium) to make a simple OBJ viewer,
//! check out [tobj viewer](https://github.com/Twinklebear/tobj_viewer). Some sample images can be found in
//! tobj viewer's readme or in [this gallery](http://imgur.com/a/xsg6v).
//!
//! The Rungholt model shown below is reasonably large (6.7M triangles, 12.3M vertices) and is loaded
//! in ~7.47s using a peak of ~1.1GB of memory on a Windows 10 machine with an i7-4790k and 16GB of
//! 1600Mhz DDR3 RAM with tobj 0.1.1 on rustc 1.6.0.
//! The model can be found on [Morgan McGuire's](http://graphics.cs.williams.edu/data/meshes.xml) meshes page and
//! was originally built by kescha. Future work will focus on improving performance and memory usage.
//!
//! <img src="http://i.imgur.com/wImyNG4.png" alt="Rungholt"
//!     style="display:block; max-width:100%; height:auto">
//!
//! For an example of integration within a ray tracer, check out tray\_rust's
//! [mesh module](https://github.com/Twinklebear/tray_rust/blob/master/src/geometry/mesh.rs).
//! The Stanford Buddha and Dragon from the
//! [Stanford 3D Scanning Repository](http://graphics.stanford.edu/data/3Dscanrep/) both load quite quickly.
//! The Rust logo model was made by
//! [Nylithius on BlenderArtists](http://blenderartists.org/forum/showthread.php?362836-Rust-language-3D-logo).
//! The materials used are from the [MERL BRDF Database](http://www.merl.com/brdf/).
//!
//! <img src="http://i.imgur.com/E1ylrZW.png" alt="Rust logo with friends"
//!     style="display:block; max-width:100%; height:auto">
//!

#![allow(dead_code)]
#![cfg_attr(all(test, feature = "unstable"), feature(test))]
#![cfg_attr(feature = "unstable", feature(plugin))]
#![cfg_attr(feature = "unstable", plugin(clippy))]

#[cfg(all(test, feature = "unstable"))] extern crate test;

#[macro_use]
extern crate log;

use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;
use std::fs::File;
use std::collections::HashMap;
use std::str::{FromStr, SplitWhitespace};

// External types
pub use types::{
    LoadResult,
    MTLLoadResult,
    LoadError,
    Material,
    Model,
    Mesh,
};
// Internal types
use types::{
    VertexIndices,
    Face,
};

mod types;

/// Parse the floatn information from the words, words is an iterator over the float strings
/// Returns false if parsing failed
fn parse_floatn(val_str: SplitWhitespace, vals: &mut Vec<f32>, n: usize) -> bool {
    let sz = vals.len();
    for p in val_str {
        if sz + n == vals.len() {
            return true;
        }
        match FromStr::from_str(p) {
            Ok(x) => vals.push(x),
            Err(_) => return false,
        }
    }
    // Require that we found the desired number of floats
    sz + n == vals.len()
}

/// Parse the float3 into the array passed, returns false if parsing failed
fn parse_float3(val_str: SplitWhitespace, vals: &mut [f32; 3]) -> bool {
    for (i, p) in val_str.enumerate() {
        match FromStr::from_str(p) {
            Ok(x) => vals[i] = x,
            Err(_) => return false,
        }
    }
    true
}

/// Parse vertex indices for a face and append it to the list of faces passed
/// Also handles relative face indices (negative values) which is why passing the number of
/// positions, texcoords and normals is required
/// returns false if an error occured parsing the face
fn parse_face(face_str: SplitWhitespace, faces: &mut Vec<Face>, pos_sz: usize, tex_sz: usize,
                  norm_sz: usize) -> bool {
    let mut indices = Vec::new();
    for f in face_str {
        match VertexIndices::parse(f, pos_sz, tex_sz, norm_sz) {
            Some(v) => indices.push(v),
            None => return false,
        }
    }
    // Check if we read a triangle or a quad face and push it on
    match indices.len() {
        3 => faces.push(Face::Triangle(indices[0], indices[1], indices[2])),
        4 => faces.push(Face::Quad(indices[0], indices[1], indices[2], indices[3])),
        _ => faces.push(Face::Polygon(indices)),
    }
    true
}

/// Add a vertex to a mesh by either re-using an existing index (eg. it's in the `index_map`)
/// or appending the position, texcoord and normal as appropriate and creating a new vertex
fn add_vertex(mesh: &mut Mesh, index_map: &mut HashMap<VertexIndices, u32>, vert: &VertexIndices,
              pos: &[f32], texcoord: &[f32], normal: &[f32]) {
    match index_map.get(vert) {
        Some(&i) => mesh.indices.push(i),
        None => {
            let v = vert.v as usize;
            // Add the vertex to the mesh
            mesh.positions.push(pos[v * 3]);
            mesh.positions.push(pos[v * 3 + 1]);
            mesh.positions.push(pos[v * 3 + 2]);
            if !texcoord.is_empty() && vert.vt > -1 {
                let vt = vert.vt as usize;
                mesh.texcoords.push(texcoord[vt * 2]);
                mesh.texcoords.push(texcoord[vt * 2 + 1]);
            }
            if !normal.is_empty() && vert.vn > -1 {
                let vn = vert.vn as usize;
                mesh.normals.push(normal[vn * 3]);
                mesh.normals.push(normal[vn * 3 + 1]);
                mesh.normals.push(normal[vn * 3 + 2]);
            }
            let next = index_map.len() as u32;
            mesh.indices.push(next);
            index_map.insert(*vert, next);
        }
    }
}

/// Export a list of faces to a mesh and return it, converting quads to tris
fn export_faces(
    pos: &[f32],
    texcoord: &[f32],
    normal: &[f32],
    faces: &[Face],
    mat_id: Option<String>,
) -> Mesh {
    let mut index_map = HashMap::new();
    let mut mesh = Mesh::empty();
    mesh.material = mat_id;
    for f in faces {
        // Optimized paths for Triangles and Quads, Polygon handles the general case of an unknown
        // length triangle fan
        match *f {
            Face::Triangle(ref a, ref b, ref c) => {
                add_vertex(&mut mesh, &mut index_map, a, pos, texcoord, normal);
                add_vertex(&mut mesh, &mut index_map, b, pos, texcoord, normal);
                add_vertex(&mut mesh, &mut index_map, c, pos, texcoord, normal);
            },
            Face::Quad(ref a, ref b, ref c, ref d) => {
                add_vertex(&mut mesh, &mut index_map, a, pos, texcoord, normal);
                add_vertex(&mut mesh, &mut index_map, b, pos, texcoord, normal);
                add_vertex(&mut mesh, &mut index_map, c, pos, texcoord, normal);

                add_vertex(&mut mesh, &mut index_map, a, pos, texcoord, normal);
                add_vertex(&mut mesh, &mut index_map, c, pos, texcoord, normal);
                add_vertex(&mut mesh, &mut index_map, d, pos, texcoord, normal);
            },
            Face::Polygon(ref indices) => {
                let a = &indices[0];
                let mut b = &indices[1];
                for c in indices.iter().skip(2) {
                    add_vertex(&mut mesh, &mut index_map, a, pos, texcoord, normal);
                    add_vertex(&mut mesh, &mut index_map, b, pos, texcoord, normal);
                    add_vertex(&mut mesh, &mut index_map, c, pos, texcoord, normal);
                    b = c;
                }
            },
        }
    }
    mesh
}

/// Load the various objects specified in the OBJ file and any associated MTL file
/// Returns a pair of Vecs containing the loaded models and materials from the file.
pub fn load_obj(file_name: &Path) -> LoadResult {
    let file = match File::open(file_name) {
        Ok(f) => f,
        Err(e) => {
            debug!("tobj::load_obj - failed to open {:?} due to {}", file_name, e);
            return Err(LoadError::OpenFileFailed);
        },
    };
    let mut reader = BufReader::new(file);
    parse_obj(&mut reader)
}

/// Load the materials defined in a MTL file
/// Returns a pair with a `Vec` holding all loaded materials and a `HashMap` containing a mapping of
/// material names to indices in the Vec.
pub fn load_mtl(file_name: &Path) -> MTLLoadResult {
    let file = match File::open(file_name) {
        Ok(f) => f,
        Err(e) => {
            debug!("tobj::load_mtl - failed to open {:?} due to {}", file_name, e);
            return Err(LoadError::OpenFileFailed);
        },
    };
    let mut reader = BufReader::new(file);
    parse_mtl(&mut reader)
}

/// Load the various meshes in an OBJ buffer. `base_path` specifies the path prefix to apply to
/// referenced material libs
pub fn parse_obj<B: BufRead>(reader: &mut B) -> LoadResult {
    let mut models = Vec::new();
    let mut material_libraries = Vec::new();

    let mut tmp_pos = Vec::new();
    let mut tmp_texcoord = Vec::new();
    let mut tmp_normal = Vec::new();
    let mut tmp_faces: Vec<Face> = Vec::new();
    // name of the current object being parsed
    let mut name = "unnamed_object".to_owned();
    // material used by the current object being parsed
    let mut mat_id = None;
    for line in reader.lines() {
        let (line, mut words) = match line {
            Ok(ref line) => (&line[..], line[..].split_whitespace()),
            Err(e) => {
                debug!("tobj::load_obj - failed to read line due to {}", e);
                return Err(LoadError::ReadError);
            },
        };
        match words.next() {
            Some("#") | None => continue,
            Some("v") => {
                if !parse_floatn(words, &mut tmp_pos, 3) {
                    return Err(LoadError::PositionParseError);
                }
            },
            Some("vt") => {
                if !parse_floatn(words, &mut tmp_texcoord, 2) {
                    return Err(LoadError::TexcoordParseError);
                }
            },
            Some("vn") => {
                if !parse_floatn(words, &mut tmp_normal, 3) {
                    return Err(LoadError::NormalParseError);
                }
            },
            Some("f") => {
                if !parse_face(words, &mut tmp_faces, tmp_pos.len() / 3, tmp_texcoord.len() / 2,
                               tmp_normal.len() / 3) {
                   return Err(LoadError::FaceParseError);
                }
            },
            // Just treating object and group tags identically. Should there be different behavior
            // for them?
            Some("o") | Some("g") => {
                // If we were already parsing an object then a new object name
                // signals the end of the current one, so push it onto our list of objects
                if !name.is_empty() && !tmp_faces.is_empty() {
                    models.push(
                        Model::new(
                            export_faces(
                                &tmp_pos,
                                &tmp_texcoord,
                                &tmp_normal,
                                &tmp_faces,
                                mat_id.take(),
                            ),
                            name
                        )
                    );
                    tmp_faces.clear();
                }
                name = line[1..].trim().to_owned();
                if name.is_empty() {
                    return Err(LoadError::InvalidObjectName);
                }
            },
            Some("mtllib") => {
                if let Some(mtllib) = words.next() {
                    material_libraries.push(mtllib.to_owned());
                } else {
                    return Err(LoadError::MaterialParseError);
                }
            },
            Some("usemtl") => {
                if let Some(mat_name) = words.next() {
                    mat_id = Some(mat_name.into());
                } else {
                    return Err(LoadError::MaterialParseError);
                }
            },
            // Just ignore unrecognized characters
            Some(_) => {},
        }
    }
    // For the last object in the file we won't encounter another object name to tell us when it's
    // done, so if we're parsing an object push the last one on the list as well
    if !name.is_empty() {
        models.push(Model::new(export_faces(&tmp_pos, &tmp_texcoord, &tmp_normal, &tmp_faces, mat_id), name));
    }
    Ok((models, material_libraries))
}

/// Load the various materials in a MTL buffer
pub fn parse_mtl<B: BufRead>(reader: &mut B) -> MTLLoadResult {
    let mut materials = HashMap::new();
    // The current material being parsed
    let mut cur_mat = Material::empty();

    for line in reader.lines() {
        let (line, mut words) = match line {
            Ok(ref line) => (&line[..], line[..].split_whitespace()),
            Err(e) => {
                debug!("tobj::load_obj - failed to read line due to {}", e);
                return Err(LoadError::ReadError);
            },
        };
        match words.next() {
            Some("#") | None => continue,
            // TODO: export these all to constants?
            Some("newmtl") => {
                // If we were passing a material save it out to our vector
                if !cur_mat.name.is_empty() {
                    materials.insert(cur_mat.name.clone(), cur_mat);
                }
                cur_mat = Material::empty();
                cur_mat.name = line["newmtl".len()..].trim().to_owned();
                if cur_mat.name.is_empty() {
                    return Err(LoadError::InvalidObjectName);
                }
            },
            Some("Ka") => {
                if !parse_float3(words, &mut cur_mat.ambient) {
                    return Err(LoadError::MaterialParseError);
                }
            },
            Some("Kd") => {
                if !parse_float3(words, &mut cur_mat.diffuse) {
                    return Err(LoadError::MaterialParseError);
                }
            },
            Some("Ks") => {
                if !parse_float3(words, &mut cur_mat.specular) {
                    return Err(LoadError::MaterialParseError);
                }
            },
            Some("Ns") => {
                if let Some(p) = words.next() {
                    match FromStr::from_str(p) {
                        Ok(x) => cur_mat.shininess = x,
                        Err(_) => return Err(LoadError::MaterialParseError),
                    }
                } else {
                    return Err(LoadError::MaterialParseError);
                }
            },
            Some("d") => {
                if let Some(p) = words.next() {
                    match FromStr::from_str(p) {
                        Ok(x) => cur_mat.dissolve = x,
                        Err(_) => return Err(LoadError::MaterialParseError),
                    }
                } else {
                    return Err(LoadError::MaterialParseError);
                }
            },
            Some("map_Ka") => {
                match words.next() {
                    Some(tex) => cur_mat.ambient_texture = tex.to_owned(),
                    None => return Err(LoadError::MaterialParseError),
                }
            },
            Some("map_Kd") => {
                match words.next() {
                    Some(tex) => cur_mat.diffuse_texture = tex.to_owned(),
                    None => return Err(LoadError::MaterialParseError),
                }
            },
            Some("map_Ks") => {
                match words.next() {
                    Some(tex) => cur_mat.specular_texture = tex.to_owned(),
                    None => return Err(LoadError::MaterialParseError),
                }
            },
            Some("map_Ns") => {
                match words.next() {
                    Some(tex) => cur_mat.normal_texture = tex.to_owned(),
                    None => return Err(LoadError::MaterialParseError),
                }
            },
            Some("map_d") => {
                match words.next() {
                    Some(tex) => cur_mat.dissolve_texture = tex.to_owned(),
                    None => return Err(LoadError::MaterialParseError),
                }
            },
            Some(unknown) => {
                if !unknown.is_empty() {
                    let param = line[unknown.len()..].trim().to_owned();
                    cur_mat.unknown_param.insert(unknown.to_owned(), param);
                }
            },
        }
    }
    // Finalize the last material we were parsing
    if !cur_mat.name.is_empty() {
        materials.insert(cur_mat.name.clone(), cur_mat);
    }
    Ok(materials)
}

/// Print out all loaded properties of some models and associated materials
pub fn print_model_info(models: &[Model], materials: &[Material]) {
    println!("# of models: {}", models.len());
    println!("# of materials: {}", materials.len());
    for (i, m) in models.iter().enumerate() {
        let mesh = &m.mesh;
        println!("model[{}].name = \'{}\'", i, m.name);
        println!("model[{}].mesh.material = {:?}", i, mesh.material);

        println!("Size of model[{}].indices: {}", i, mesh.indices.len());
        for f in 0..mesh.indices.len() / 3 {
            println!("    idx[{}] = {}, {}, {}.", f, mesh.indices[3 * f], mesh.indices[3 * f + 1],
                     mesh.indices[3 * f + 2]);
        }

        println!("model[{}].vertices: {}", i, mesh.positions.len() / 3);
        println!("model[{}].normals: {}", i, mesh.normals.len() / 3);
        println!("model[{}].texcoords: {}", i, mesh.texcoords.len() / 2);
        assert!(mesh.positions.len() % 3 == 0);
        assert!(mesh.normals.len() % 3 == 0);
        assert!(mesh.texcoords.len() % 2 == 0);
        for v in 0..mesh.positions.len() / 3 {
            println!("    v[{}] = ({}, {}, {})", v, mesh.positions[3 * v], mesh.positions[3 * v + 1],
                    mesh.positions[3 * v + 2]);
            if !mesh.normals.is_empty() {
                println!("    vn[{}] = ({}, {}, {})", v, mesh.normals[3 * v], mesh.normals[3 * v + 1],
                        mesh.normals[3 * v + 2]);
            }
            if !mesh.texcoords.is_empty() {
                println!("    vt[{}] = ({}, {})", v, mesh.texcoords[2 * v], mesh.texcoords[2 * v + 1]);
            }
        }
    }
    print_material_info(materials);
}

/// Print out all loaded properties of some materials
pub fn print_material_info(materials: &[Material]) {
    for (i, m) in materials.iter().enumerate() {
        println!("material[{}].name = \'{}\'", i, m.name);
        println!("    material.Ka = ({}, {}, {})", m.ambient[0], m.ambient[1], m.ambient[2]);
        println!("    material.Kd = ({}, {}, {})", m.diffuse[0], m.diffuse[1], m.diffuse[2]);
        println!("    material.Ks = ({}, {}, {})", m.specular[0], m.specular[1], m.specular[2]);
        println!("    material.Ns = {}", m.shininess);
        println!("    material.d = {}", m.dissolve);
        println!("    material.map_Ka = {}", m.ambient_texture);
        println!("    material.map_Kd = {}", m.diffuse_texture);
        println!("    material.map_Ks = {}", m.specular_texture);
        println!("    material.map_Ns = {}", m.normal_texture);
        println!("    material.map_d = {}", m.dissolve_texture);
        for (k, v) in &m.unknown_param {
            println!("    material.{} = {}", k, v);
        }
    }
}

#[cfg(all(test, feature = "unstable"))]
mod benches {
    use test::Bencher;
    use std::path::Path;
    use super::load_obj;

    #[bench]
    fn bench_cornell(b: &mut Bencher) {
        let path = Path::new("cornell_box.obj");
        b.iter(|| {
            let m = load_obj(path);
            assert!(m.is_ok());
            m.is_ok()
        });
    }
}
