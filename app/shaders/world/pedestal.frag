#version 330 core

uniform vec3 uCamera;


in      vec3 vPosition;
in      vec3 vNormal;
in      vec2 vUV;


out     vec4 fragColor;

void main() {


  vec3 col = vec3(.1 );

    if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
      col *= 2.;
    }
    fragColor = vec4( col , 1.);

}