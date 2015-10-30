#version 330 core

uniform vec3 uEye;
uniform vec3 uLight;


in      vec3 vPosition;
in      vec3 vNormal;
in      vec2 vUV;


out     vec4 fragColor;


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
  return pNoise( p * 100. );
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

    vec3 light = vPosition - uLight;
    vec3 lightDir = normalize( light );

  /*  vec3 nNorm = calcNormal( vPosition );

    nNorm *= .1;
    nNorm += vNormal;
    nNorm = normalize( nNorm );*/

    vec3 nNorm = -vNormal;

    vec3 refl = reflect( lightDir , nNorm );
    vec3 eyeDir = normalize( uEye - vPosition );

    float spec = max( 0. , dot( eyeDir, refl ));


    float lPower = 30. / pow( length( light ), 5.);
    float lDot = dot( nNorm , lightDir );

    vec3 col = vec3( lDot * lPower) + vec3( lPower ) + vec3( 100. * pow(spec, 20. ));

    if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
      col += vec3(.4);
    }
    fragColor = vec4( col * .5, 1.);

}