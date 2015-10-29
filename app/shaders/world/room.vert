#version 330 core

uniform mat4 uModel;
uniform mat4 uViewProjection;
uniform mat4 uModelViewProjection;
uniform mat4 uNormalMatrix;
uniform mat4 uInverseModel;
uniform vec3 uRepelPosition;


in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec2 vUV;


float hash( float n ) { return fract(sin(n)*753.5453123); }
float pNoise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
  
    float n = p.x + p.y*157.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}


float map( in vec3 p ){
  return pNoise( p * 10. );
}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy) - map(pos-eps.xyy),
      map(pos+eps.yxy) - map(pos-eps.yxy),
      map(pos+eps.yyx) - map(pos-eps.yyx) );

  return normalize(nor);
}



void main() {

    // Pass some variables to the fragment shader
    vec3 pos = vec3(uModel * vec4(aPosition, 1.0));

    float power = map( pos );


    vec3 norm = aNormal + .2 * calcNormal( pos );
    norm = normalize( norm );

    // If scaled not uniformly, 
    // this will screw up ( i think ... )
    vNormal   = vec3(uModel * vec4(norm, 0.0));

    pos += vNormal * ( power) * 1.3 * max( 0., (.5 - length( aUV - vec2( .5 ))));

    vPosition = pos;
    vUV = aUV;

    // If scaled not uniformly, 
    // this will screw up ( i think ... )
    vNormal   = vec3(uModel * vec4(norm, 0.0));

    gl_Position = uViewProjection * vec4( pos, 1.0);

}