include {fastp} from "${params.pipeline_dir}/software/26839459-6d65-4457-810c-dc3fd43c07f9/main.nf"
include {bowtie2_index} from "${params.pipeline_dir}/software/f7a883e8-efb5-499b-92a4-93f32acf820e/main.nf"
include {bowtie2} from "${params.pipeline_dir}/software/fd4b5653-dda5-4ad2-af67-4bbd3a05f36f/main.nf"


workflow{
    ch_input = channel.fromList(params.raw_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    fastp(ch_input )
    ch_bowtie2_index_out =  bowtie2_index([[id:params.fasta.file_name],params.fasta.fa])

    // ch_bowtie2_index_out.index.view()
    ch_bowtie2_index_out
    .map { meta, files ->
        files[0].toString().replace('.1.bt2','')
    }
    .set { ch_bowtie2_prefix }
    ch_bowtie2_prefix.view()
    // fastp.out.reads.view()
    bowtie2(fastp.out.reads, ch_bowtie2_prefix)

}