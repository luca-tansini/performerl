#include <erl_nif.h>
#include <stdio.h>

/* The mandatory tracing NIFs: */
static ERL_NIF_TERM enabled(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"enabled", 3, enabled},
    {"trace", 5, trace}};

ERL_NIF_INIT(performerl_custom_meta_tracer, nif_funcs, NULL, NULL, NULL, NULL)

/*
 * argv[0]: TraceTag
 * argv[1]: TracerState
 * argv[2]: Tracee
 */
static ERL_NIF_TERM enabled(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

    ErlNifPid tracer;
    if (enif_get_local_pid(env, argv[1], &tracer))
        if (!enif_is_process_alive(env, &tracer))
        {
            // trace_status is a special type of TraceTag, which is
            // used to check if the tracer is still to be active
            if (enif_is_identical(enif_make_atom(env, "trace_status"), argv[0]))
                /* tracer is dead so we should remove this tracepoint */
                return enif_make_atom(env, "remove");
            else
                // tracer is dead so we should ignore this tracepoint
                // (will be removed by trace_status)
                return enif_make_atom(env, "discard");
        }

    /* Only generate trace for when tracer != tracee */
    if (enif_is_identical(argv[1], argv[2]))
        return enif_make_atom(env, "discard");

    /* Only trigger trace messages on 'call' */
    if (enif_is_identical(enif_make_atom(env, "call"), argv[0]))
        return enif_make_atom(env, "trace");

    /* Have to answer trace_status */
    if (enif_is_identical(enif_make_atom(env, "trace_status"), argv[0]))
        return enif_make_atom(env, "trace");

    return enif_make_atom(env, "discard");
}

/*
 * argv[0]: TraceTag: should only be 'call' in our case
 * argv[1]: TracerState: tracer, the process to send trace message to
 * argv[2]: Tracee: the caller's PID in our case
 * argv[3]: TraceTerm: In our case is going to be {M, F, Args}
 * argv[4]: Options, map containing some options, not interesting in our case
 */
static ERL_NIF_TERM trace(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid tracer;
    ERL_NIF_TERM ms_res, trace_msg, arity, MFArity;
    const ERL_NIF_TERM *MFA;
    unsigned int length;
    int MFAlength;

    if (enif_get_local_pid(env, argv[1], &tracer))
    {
        enif_get_tuple(env, argv[3], &MFAlength, &MFA);
        enif_get_list_length(env, MFA[2], &length);
        arity = enif_make_int(env, length);
        MFArity = enif_make_tuple3(env, MFA[0], MFA[1], arity);
        trace_msg = enif_make_tuple3(env, enif_make_atom(env, "custom_trace"),
                                     argv[2], MFArity);
        enif_send(env, &tracer, NULL, trace_msg);
    }

    return enif_make_atom(env, "ok");
}
