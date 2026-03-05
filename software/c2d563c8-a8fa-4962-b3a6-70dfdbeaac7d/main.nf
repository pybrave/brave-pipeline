process simulate_reads {

    publishDir "output/ngsngs",  mode: 'symlink', overwrite: true

    container 'wybioinfo/ngsngs'

    input:
    tuple val(sample_id), path(ref)

    output:
    tuple val(sample_id), path("${sample_id}*"), path("${sample_id}*")

    script:
    """
    ngsngs \
      -i ${ref} \
      -r ${params.reads_number} \
      -t ${task.cpus} \
      -s 1 \
      -l 150 \
      -qs 40 \
      -seq PE \
      -f ${params.formats} \
      -o ${sample_id}
    """
}

workflow{
    sample_ids = (1..params.sample_size).collect { "${params.prefix}-${it}" }
    // print(sample_ids)
    ch_samples = Channel.from(sample_ids)
        .map { sample_id -> tuple(sample_id, file(params.fasta.fa)) }
    ch_samples.view()
    simulate_reads(ch_samples)
}