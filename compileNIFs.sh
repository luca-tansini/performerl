echo "===> Compiling NIFs"
gcc -I /home/tanso/Desktop/kerl/20.3.8.26/erts-9.3.3.15/include -o src/agents/performerl_custom_meta_tracer.so -fPIC -shared src/agents/performerl_custom_meta_tracer.c
