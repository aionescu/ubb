#version 330 core
in vec2 bTex;
out vec4 FragColor;
uniform sampler2D textura_mea;
void main()
{
    FragColor = texture(textura_mea,bTex);
}
