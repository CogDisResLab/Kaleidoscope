<script lang="ts">
	import '../layout.css';
	import favicon from '$lib/assets/favicon.svg';

	let { children } = $props();
</script>

<svelte:head><link rel="icon" href={favicon} /></svelte:head>

<div class="m-8">
  <div class="flex flex-col gap-2 max-w-md">
    <label class="input validator border-base-300">
      <svg class="h-[1em] opacity-50" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><g stroke-linejoin="round" stroke-linecap="round" stroke-width="2.5" fill="none" stroke="currentColor"><path d="M15.5 14h-.79l-.28-.27a6.5 6.5 0 001.48-5.34c-.47-2.78-2.79-5-5.59-5.34a6.505 6.505 0 00-7.27 7.27c.34 2.8 2.56 5.12 5.34 5.59a6.5 6.5 0 005.34-1.48l.27.28v.79l4.25 4.25c.41.41 1.08.41 1.49 0 .41-.41.41-1.08 0-1.49L15.5 14zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z"></path></g></svg>
      
      <input
        type="text"
        required
        placeholder="AKT1,RSK4,BRCA1"
        pattern="^[A-Za-z0-9-]{2,10}(,[A-Za-z0-9-]{2,10})*$"
        class="grow"
      />
    </label>
    
    <p class="validator-hint text-xs">
      Invalid format. Use comma-separated symbols (2-10 chars) with <b>no spaces</b>.
    </p>

    <label class="label">
      <span class="label-text-alt text-base-content/40 italic">
        Example: TP53,EGFR,MYC
      </span>
    </label>
  </div>
</div>
<p class="validator-hint">
  Must be 2 to 10 characters containing only <br/>
  letters, numbers, commas, or dash, with no spaces
</p>
{@render children()}

