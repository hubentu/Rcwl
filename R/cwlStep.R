#' cwlStep function
#' @description Constructor function for `cwlStep` object.
#' @param id A user-defined unique identifier for this workflow step.
#' @param run A `cwlProcess` object for command line tool, or path to
#'     a CWL file.
#' @param In A list of input parameters which will be constructed into
#'     `stepInParamList`.
#' @param Out A list of outputs.
#' @param scatter character or a list. The inputs to be scattered.
#' @param scatterMethod required if scatter is an array of more than
#'     one element. It can be one of "dotproduct",
#'     "nested_crossproduct" and "flat_crossproduct".
#' @param label A short, human-readable label of this object.
#' @param doc A documentation string for this object, or an array of
#'     strings which should be concatenated.
#' @param requirements Requirements that apply to either the runtime
#'     environment or the workflow engine.
#' @param hints Hints applying to either the runtime environment or
#'     the workflow engine.
#' @param when If defined, only run the step when the expression
#'     evaluates to true. If false the step is skipped.
#' @export
#' @return An object of class `cwlStep`.
#' @details For more details:
#'     https://www.commonwl.org/v1.0/Workflow.html#WorkflowStep
#' @examples
#' s1 <- cwlStep(id = "s1")
#' @seealso \code{\link{cwlWorkflow}}
cwlStep <- function(id, run = cwlProcess(),
                    In = list(), Out = list(),
                    scatter = character(), scatterMethod = character(),
                    label = character(),
                    doc = character(),
                    requirements = list(),
                    hints = list(),
                    when = character()) {

    ## add defaults to In
    ins <- names(inputs(run))
    inL <- paste0(id, "_", ins)
    names(inL) <- ins
    if(length(In)>0){
        inL[match(names(In), ins)] <- In
    }
    ## remove optional/default
    df <- unlist(lapply(inputs(run),function(x){
        grepl("\\?", x@type) | length(x@default)!=0}
        ))
    if(any(df)){
        df <- setdiff(names(inL)[df], names(In))
        if(length(df)>0){
            inL <- inL[-match(df, names(inL))]
        }
    }
    In <- inL

    if(is(run, "cwlProcess")){
        if(length(ins) > 0){
            stopifnot(names(In) %in% names(inputs(run)))
        }
        sout <- as.list(names(outputs(run)))
    }else if(is(run, "character")){
        stopifnot(file.exists(run))
        clist <- read_yaml(run)
        stopifnot(names(In) %in% names(clist$inputs))
        sout <- as.list(names(clist$outputs))
    }
    slist <- list()
    for(i in seq(In)) {
        if(is.list(In[[i]])) {
            si <- stepInParam(id = names(In)[i])
            for(j in seq(In[[i]])){
                slot(si, names(In[[i]])[j]) <- In[[i]][[j]]
            }
        }else{
            si <- stepInParam(id = names(In)[i],
                              source = In[[i]])
        }
        slist[[i]] <- si
    }
    names(slist) <- names(In)
    new("cwlStep",
        id = id,
        run = run,
        In = do.call(stepInParamList, slist),
        Out = sout,
        scatter = scatter,
        scatterMethod = scatterMethod,
        label = label,
        doc = doc,
        requirements = requirements,
        hints = hints,
        when = when)
}


#' stepInputs
#' 
#' @description prepare inputs for workflow from `cwlStep` objects
#' @param stepList a list of `cwlStep` objects.
#' @return InputParamList.
#' @export
stepInputs <- function(stepList){
    ## keep unchanged inputs
    ss <- lapply(stepList, function(s){
        unlist(lapply(s@In, function(x)x@source))
    })
    ss <- unlist(ss)
    ## update ids
    ins <- do.call(c, lapply(stepList, function(x){
        i1 <- inputs(x@run)
        for(i in seq(i1)){
            i1[[i]]@id <- paste0(x@id, "_", i1[[i]]@id)
        }
        names(i1) <- paste0(x@id, "_", names(i1))        
        i1
    }))
    idx <- names(ins) %in% ss
    return(ins[idx])
}


#' stepOutputs
#' 
#' @description prepare outputs for workflow from `cwlStep` objects
#' @param stepList a list of `cwlStep` objects.
#' @return OutputParamList.
#' @export
stepOutputs <- function(stepList){
    os <- lapply(stepList, function(s1){
        lapply(outputs(s1@run), sid = s1@id, function(x, sid){
            OutputParam(id = paste0(sid, "_", x@id), type = x@type,
                        outputSource = paste0(sid, "/", x@id))
        })
        
    })
    outs <- do.call(OutputParamList, unlist(os))
    outs <- outs[-match("output", names(outs))]
    return(outs)
} 
