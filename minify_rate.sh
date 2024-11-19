#!/bin/zsh

# Check if destination directory is provided
if [[ -z "$1" ]]; then
  echo "Please provide the destination directory as an argument."
  exit 1
fi

# Initialize counters
true_count=0
false_count=0
progress=0

# Get the list of .js files in the destination directory
files=("$1"/*.js)
total_files=${#files[@]}

# Process each file
for ((i = 0; i < total_files; i++)); do
  file="${files[i]}"

  # Run minify-runner and capture output
  output=$(minify-runner -v swc@1.4.6 -d -f "$file" 2>/dev/null)

  # Get the last line of the output (true or false)
  last_line=$(echo "$output" | tail -n 1)

  # Increment counters based on last line
  if [[ "$last_line" == "true" ]]; then
    ((true_count++))
  else
    ((false_count++))
  fi

  # Calculate the current progress
  current_progress=$(( (i + 1) * 1000 / total_files ))

  # Calculate true rate and false rate only if there is at least one result
  if (( true_count + false_count > 0 )); then
    true_rate=$(( (true_count * 100) / (true_count + false_count) ))
    false_rate=$(( (false_count * 100) / (true_count + false_count) ))
  else
    true_rate=0
    false_rate=0
  fi

  # Update output if progress increased by 0.1%
  if (( current_progress > progress )); then
    progress=$current_progress
    echo -ne "Progress: $(echo "scale=1; $progress / 10" | bc)% - True count: $true_count ($true_rate%) - False count: $false_count ($false_rate%)\r"
  fi
done

# Final output with 100% progress
echo -e "\nCompleted! Final True count: $true_count ($true_rate%) - False count: $false_count ($false_rate%)"
