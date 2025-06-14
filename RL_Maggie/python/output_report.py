"""
output_report.py

This script generates a report of all output files created by the analysis pipeline.
Run this script after running the pipeline to get a summary of all outputs.
"""

import os
import json
from pathlib import Path
import pandas as pd
from datetime import datetime

# Import setup module to get output directories
import importlib
setup_mod = importlib.import_module('00_setup')

# Get output directories
OUTPUT_DIR = setup_mod.OUTPUT_DIR
TRIAL_DATA_DIR = setup_mod.TRIAL_DATA_DIR
ESTIMATION_DATA_DIR = setup_mod.ESTIMATION_DATA_DIR
ANALYSIS_RESULTS_DIR = setup_mod.ANALYSIS_RESULTS_DIR
REPORTS_DIR = setup_mod.REPORTS_DIR

def get_file_info(file_path):
    """Get information about a file"""
    file_info = {}
    file_info['path'] = str(file_path)
    file_info['size_kb'] = file_path.stat().st_size / 1024
    file_info['last_modified'] = datetime.fromtimestamp(file_path.stat().st_mtime).strftime('%Y-%m-%d %H:%M:%S')
    
    # Get basic info about the file content
    if file_path.suffix.lower() == '.csv':
        try:
            df = pd.read_csv(file_path)
            file_info['rows'] = len(df)
            file_info['columns'] = list(df.columns)
            file_info['column_count'] = len(df.columns)
        except Exception as e:
            file_info['error'] = str(e)
    elif file_path.suffix.lower() == '.json':
        try:
            with open(file_path, 'r') as f:
                data = json.load(f)
            if isinstance(data, dict):
                file_info['keys'] = list(data.keys())
                file_info['key_count'] = len(data.keys())
            elif isinstance(data, list):
                file_info['list_length'] = len(data)
        except Exception as e:
            file_info['error'] = str(e)
    elif file_path.suffix.lower() in ['.png', '.jpg', '.jpeg']:
        file_info['type'] = 'image'
    
    return file_info

def generate_directory_report(directory, output_report_file=None):
    """Generate a report of all files in a directory"""
    report = {
        'directory': str(directory),
        'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
        'file_count': 0,
        'total_size_kb': 0,
        'files': []
    }
    
    if not directory.exists():
        report['error'] = f"Directory does not exist: {directory}"
        return report
    
    # Process all files in the directory
    for file_path in directory.glob('**/*'):
        if file_path.is_file():
            file_info = get_file_info(file_path)
            report['files'].append(file_info)
            report['file_count'] += 1
            report['total_size_kb'] += file_info.get('size_kb', 0)
    
    # Sort files by last modified time
    report['files'] = sorted(report['files'], key=lambda x: x.get('last_modified', ''), reverse=True)
    
    # Save report if requested
    if output_report_file:
        with open(output_report_file, 'w') as f:
            json.dump(report, f, indent=2)
    
    return report

def print_report_summary(report):
    """Print a summary of the report"""
    print(f"Report for {report['directory']}")
    print(f"Generated at: {report['timestamp']}")
    print(f"Total files: {report['file_count']}")
    print(f"Total size: {report['total_size_kb']:.2f} KB")
    print("\nFile overview:")
    
    # Group files by directory
    files_by_dir = {}
    for file_info in report['files']:
        file_path = Path(file_info['path'])
        dir_name = file_path.parent.name
        if dir_name not in files_by_dir:
            files_by_dir[dir_name] = []
        files_by_dir[dir_name].append(file_info)
    
    # Print summary by directory
    for dir_name, files in files_by_dir.items():
        print(f"\n📁 {dir_name}/ ({len(files)} files)")
        for file_info in files:
            file_path = Path(file_info['path'])
            size = file_info.get('size_kb', 0)
            rows = file_info.get('rows', '')
            if rows:
                print(f"  📄 {file_path.name} ({size:.1f} KB, {rows} rows)")
            else:
                print(f"  📄 {file_path.name} ({size:.1f} KB)")

if __name__ == "__main__":
    # Generate report for the entire output directory
    print("Generating report for all output files...")
    report = generate_directory_report(OUTPUT_DIR, REPORTS_DIR / 'output_report.json')
    print_report_summary(report)
    print(f"\nDetailed report saved to {REPORTS_DIR / 'output_report.json'}")
