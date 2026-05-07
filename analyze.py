import json
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import argparse
from pathlib import Path
from typing import Dict, List, Tuple
import glob

class FinanceAnalyzer:
    """Analyze and visualize finance simulation results."""
    
    def __init__(self):
        self.simulations = {}
        self.account_types = ['_brokerage', '_cash', '_emergencyFund', '_roth401k', '_trad401k']
        
    def load_simulation(self, filepath: str, name: str = None):
        """Load a simulation result from JSON file."""
        if name is None:
            name = Path(filepath).stem
            
        with open(filepath, 'r') as f:
            data = json.load(f)
        
        self.simulations[name] = self._process_data(data)
        print(f"✓ Loaded '{name}' with {len(data)} months of data")
        return self
    
    def load_multiple(self, pattern: str):
        """Load all JSON files matching a pattern."""
        files = glob.glob(pattern)
        if not files:
            print(f"Warning: No files found matching pattern '{pattern}'")
            return self
        
        print(f"\nFound {len(files)} simulation files:")
        for filepath in files:
            self.load_simulation(filepath)
        return self
        
    def _process_data(self, data: List[Dict]) -> pd.DataFrame:
        """Convert JSON data to structured DataFrame."""
        records = []
        for month_data in data:
            record = {
                'month': month_data['_month'],
                'income': month_data['_income'],
                'salary': month_data['_salary'],
                'taxes': month_data['_taxes']
            }
            
            # Extract account data
            for acc_type in self.account_types:
                if acc_type in month_data:
                    acc = month_data[acc_type]
                    prefix = acc_type.replace('_', '')
                    record[f'{prefix}_balance'] = acc['_balance']
                    record[f'{prefix}_contributions'] = acc['_contributions']
                    record[f'{prefix}_gains'] = acc['_gains']
            
            records.append(record)
        
        df = pd.DataFrame(records)
        df['month'] = pd.to_datetime(df['month'])
        
        # Calculate totals
        balance_cols = [col for col in df.columns if col.endswith('_balance')]
        df['total_balance'] = df[balance_cols].sum(axis=1)
        
        contribution_cols = [col for col in df.columns if col.endswith('_contributions')]
        df['total_contributions'] = df[contribution_cols].sum(axis=1)
        
        gain_cols = [col for col in df.columns if col.endswith('_gains')]
        df['total_gains'] = df[gain_cols].sum(axis=1)
        
        # Calculate tax-advantaged total
        df['tax_advantaged_balance'] = (
            df.get('roth401k_balance', 0) + 
            df.get('trad401k_balance', 0)
        )
        
        # Calculate taxable total
        df['taxable_balance'] = (
            df.get('brokerage_balance', 0) + 
            df.get('cash_balance', 0)
        )
        
        return df
    
    def calculate_score(self, df: pd.DataFrame, weights: Dict = None) -> float:
        """
        Calculate an optimization score for a simulation strategy.
        Higher score = better strategy.
        
        Default weights:
        - Final total balance: 40%
        - Total gains: 25%
        - Tax-advantaged ratio: 20%
        - Return on investment: 15%
        """
        if weights is None:
            weights = {
                'final_balance': 0.40,
                'total_gains': 0.25,
                'tax_advantaged_ratio': 0.20,
                'roi': 0.15
            }
        
        final = df.iloc[-1]
        
        # Normalize metrics (0-100 scale for comparison)
        final_balance = final['total_balance']
        total_gains = final['total_gains']
        tax_adv_ratio = (final['tax_advantaged_balance'] / final['total_balance'] * 100) if final['total_balance'] > 0 else 0
        roi = (final['total_gains'] / final['total_contributions'] * 100) if final['total_contributions'] > 0 else 0
        
        # Calculate weighted score
        score = (
            weights['final_balance'] * (final_balance / 1000) +  # Scale down for balance
            weights['total_gains'] * (total_gains / 100) +
            weights['tax_advantaged_ratio'] * tax_adv_ratio +
            weights['roi'] * roi
        )
        
        return score
    
    def find_best_strategy(self, weights: Dict = None, verbose: bool = True):
        """
        Analyze all loaded simulations and rank them.
        Returns DataFrame with rankings and detailed metrics.
        """
        if not self.simulations:
            print("No simulations loaded!")
            return None
        
        results = []
        
        for name, df in self.simulations.items():
            final = df.iloc[-1]
            
            score = self.calculate_score(df, weights)
            
            metrics = {
                'Strategy': name,
                'Score': score,
                'Final Balance': final['total_balance'],
                'Total Gains': final['total_gains'],
                'Total Contributions': final['total_contributions'],
                'ROI (%)': (final['total_gains'] / final['total_contributions'] * 100) if final['total_contributions'] > 0 else 0,
                'Tax-Advantaged (%)': (final['tax_advantaged_balance'] / final['total_balance'] * 100) if final['total_balance'] > 0 else 0,
                'Roth 401k': final.get('roth401k_balance', 0),
                'Trad 401k': final.get('trad401k_balance', 0),
                'Brokerage': final.get('brokerage_balance', 0),
                'Cash': final.get('cash_balance', 0),
                'Emergency Fund': final.get('emergencyFund_balance', 0),
                'Total Taxes': df['taxes'].sum(),
                'Duration (months)': len(df)
            }
            
            results.append(metrics)
        
        ranking_df = pd.DataFrame(results)
        ranking_df = ranking_df.sort_values('Score', ascending=False).reset_index(drop=True)
        ranking_df.index += 1  # Start ranking from 1
        
        if verbose:
            self._print_ranking_report(ranking_df)
        
        return ranking_df
    
    def _print_ranking_report(self, ranking_df: pd.DataFrame):
        """Print a formatted ranking report."""
        print("\n" + "="*80)
        print("STRATEGY RANKING REPORT")
        print("="*80)
        
        for idx, row in ranking_df.iterrows():
            print(f"\n#{idx}. {row['Strategy']}")
            print(f"   Score: {row['Score']:.2f}")
            print(f"   Final Balance: ${row['Final Balance']:,.2f}")
            print(f"   Total Gains: ${row['Total Gains']:,.2f}")
            print(f"   ROI: {row['ROI (%)']:.2f}%")
            print(f"   Tax-Advantaged: {row['Tax-Advantaged (%)']:.1f}%")
            print(f"   Total Taxes: ${row['Total Taxes']:,.2f}")
        
        print("\n" + "="*80)
        print(f"WINNER: {ranking_df.iloc[0]['Strategy']}")
        print("="*80)
        
        # Show what makes the winner best
        winner = ranking_df.iloc[0]
        print("\nWhy this strategy wins:")
        if winner['Final Balance'] == ranking_df['Final Balance'].max():
            print("  ✓ Highest final balance")
        if winner['Total Gains'] == ranking_df['Total Gains'].max():
            print("  ✓ Highest total gains")
        if winner['ROI (%)'] == ranking_df['ROI (%)'].max():
            print("  ✓ Best return on investment")
        if winner['Tax-Advantaged (%)'] == ranking_df['Tax-Advantaged (%)'].max():
            print("  ✓ Best tax-advantaged allocation")
    
    def plot_account_balances(self, sim_name: str = None, figsize=(14, 8)):
        """Plot account balances over time for a simulation."""
        if sim_name is None:
            sim_name = list(self.simulations.keys())[0]
        
        df = self.simulations[sim_name]
        
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=figsize)
        
        # Plot 1: Individual accounts
        accounts = {
            'Brokerage': 'brokerage_balance',
            'Cash': 'cash_balance',
            'Emergency Fund': 'emergencyFund_balance',
            'Roth 401k': 'roth401k_balance',
            'Traditional 401k': 'trad401k_balance'
        }
        
        for label, col in accounts.items():
            if col in df.columns:
                ax1.plot(df['month'], df[col], marker='o', label=label, linewidth=2)
        
        ax1.set_xlabel('Date')
        ax1.set_ylabel('Balance ($)')
        ax1.set_title(f'Account Balances Over Time - {sim_name}')
        ax1.legend()
        ax1.grid(True, alpha=0.3)
        ax1.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'${x:,.0f}'))
        
        # Plot 2: Total balance with contributions vs gains
        ax2.plot(df['month'], df['total_balance'], marker='o', 
                label='Total Balance', linewidth=2.5, color='black')
        ax2.plot(df['month'], df['total_contributions'], marker='s', 
                label='Total Contributions', linewidth=2, color='blue', linestyle='--')
        ax2.plot(df['month'], df['total_gains'], marker='^', 
                label='Total Gains', linewidth=2, color='green', linestyle='--')
        
        ax2.set_xlabel('Date')
        ax2.set_ylabel('Amount ($)')
        ax2.set_title('Total Portfolio: Balance vs Contributions vs Gains')
        ax2.legend()
        ax2.grid(True, alpha=0.3)
        ax2.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'${x:,.0f}'))
        
        plt.tight_layout()
        return fig
    
    def compare_simulations(self, figsize=(14, 10)):
        """Compare multiple simulations side by side."""
        if len(self.simulations) < 2:
            print("Need at least 2 simulations to compare")
            return
        
        fig, axes = plt.subplots(2, 2, figsize=figsize)
        
        # Plot 1: Total balance comparison
        for name, df in self.simulations.items():
            axes[0, 0].plot(df['month'], df['total_balance'], 
                          marker='o', label=name, linewidth=2)
        axes[0, 0].set_title('Total Balance Comparison')
        axes[0, 0].set_ylabel('Balance ($)')
        axes[0, 0].legend()
        axes[0, 0].grid(True, alpha=0.3)
        axes[0, 0].yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'${x:,.0f}'))
        
        # Plot 2: Total gains comparison
        for name, df in self.simulations.items():
            axes[0, 1].plot(df['month'], df['total_gains'], 
                          marker='o', label=name, linewidth=2)
        axes[0, 1].set_title('Total Gains Comparison')
        axes[0, 1].set_ylabel('Gains ($)')
        axes[0, 1].legend()
        axes[0, 1].grid(True, alpha=0.3)
        axes[0, 1].yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'${x:,.0f}'))
        
        # Plot 3: Tax-advantaged vs Taxable
        for name, df in self.simulations.items():
            tax_adv_pct = (df['tax_advantaged_balance'] / df['total_balance'] * 100).fillna(0)
            axes[1, 0].plot(df['month'], tax_adv_pct, marker='o', label=name, linewidth=2)
        axes[1, 0].set_title('Tax-Advantaged Allocation (%)')
        axes[1, 0].set_ylabel('% of Total Portfolio')
        axes[1, 0].set_xlabel('Date')
        axes[1, 0].legend()
        axes[1, 0].grid(True, alpha=0.3)
        axes[1, 0].axhline(y=50, color='gray', linestyle=':', alpha=0.5)
        
        # Plot 4: Final values bar chart
        final_values = {}
        for name, df in self.simulations.items():
            final_values[name] = df['total_balance'].iloc[-1]
        
        # Sort by value
        sorted_items = sorted(final_values.items(), key=lambda x: x[1], reverse=True)
        names = [item[0] for item in sorted_items]
        values = [item[1] for item in sorted_items]
        colors = plt.cm.viridis(np.linspace(0, 0.8, len(names)))
        
        bars = axes[1, 1].bar(range(len(names)), values, color=colors)
        axes[1, 1].set_title('Final Total Balance Ranking')
        axes[1, 1].set_ylabel('Balance ($)')
        axes[1, 1].set_xlabel('Strategy')
        axes[1, 1].set_xticks(range(len(names)))
        axes[1, 1].set_xticklabels(names, rotation=45, ha='right')
        axes[1, 1].yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'${x:,.0f}'))
        
        # Add value labels on bars
        for bar in bars:
            height = bar.get_height()
            axes[1, 1].text(bar.get_x() + bar.get_width()/2., height,
                          f'${height:,.0f}',
                          ha='center', va='bottom', fontsize=8)
        
        plt.tight_layout()
        return fig
    
    def summary_stats(self, sim_name: str = None) -> pd.DataFrame:
        """Generate summary statistics for a simulation."""
        if sim_name is None:
            sim_name = list(self.simulations.keys())[0]
        
        df = self.simulations[sim_name]
        final = df.iloc[-1]
        
        stats = {
            'Simulation': sim_name,
            'Duration (months)': len(df),
            'Final Total Balance': final['total_balance'],
            'Total Contributions': final['total_contributions'],
            'Total Gains': final['total_gains'],
            'Total Return (%)': (final['total_gains'] / final['total_contributions'] * 100) if final['total_contributions'] > 0 else 0,
            'Final Brokerage': final.get('brokerage_balance', 0),
            'Final Cash': final.get('cash_balance', 0),
            'Final Emergency Fund': final.get('emergencyFund_balance', 0),
            'Final Roth 401k': final.get('roth401k_balance', 0),
            'Final Trad 401k': final.get('trad401k_balance', 0),
            'Total Taxes Paid': df['taxes'].sum()
        }
        
        return pd.Series(stats)
    
    def plot_allocation_pie(self, sim_name: str = None, month_index: int = -1):
        """Plot pie chart of account allocation at a specific time."""
        if sim_name is None:
            sim_name = list(self.simulations.keys())[0]
        
        df = self.simulations[sim_name]
        row = df.iloc[month_index]
        
        accounts = {
            'Brokerage': row.get('brokerage_balance', 0),
            'Cash': row.get('cash_balance', 0),
            'Emergency Fund': row.get('emergencyFund_balance', 0),
            'Roth 401k': row.get('roth401k_balance', 0),
            'Traditional 401k': row.get('trad401k_balance', 0)
        }
        
        # Filter out zero balances
        accounts = {k: v for k, v in accounts.items() if v > 0}
        
        fig, ax = plt.subplots(figsize=(10, 8))
        colors = plt.cm.Set3(range(len(accounts)))
        
        wedges, texts, autotexts = ax.pie(
            accounts.values(),
            labels=accounts.keys(),
            autopct='%1.1f%%',
            colors=colors,
            startangle=90
        )
        
        # Improve text
        for text in texts:
            text.set_fontsize(12)
        for autotext in autotexts:
            autotext.set_color('white')
            autotext.set_fontsize(10)
            autotext.set_weight('bold')
        
        ax.set_title(f'Account Allocation - {sim_name}\n'
                    f'Date: {row["month"].strftime("%Y-%m")}\n'
                    f'Total: ${row["total_balance"]:,.2f}',
                    fontsize=14)
        
        return fig


def main():
    parser = argparse.ArgumentParser(
        description='Analyze and compare finance simulation results',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Analyze a single simulation
  python finance_analyzer.py out.json
  
  # Compare multiple simulations
  python finance_analyzer.py strategy1.json strategy2.json strategy3.json
  
  # Use wildcards to load all simulations
  python finance_analyzer.py simulations/*.json
  
  # Find best strategy with custom output directory
  python finance_analyzer.py *.json --find-best --output results/
  
  # Customize scoring weights
  python finance_analyzer.py *.json --find-best --weight-balance 0.5 --weight-roi 0.3
        """
    )
    
    parser.add_argument('files', nargs='+', help='JSON simulation files to analyze')
    parser.add_argument('--output', '-o', default='./output', 
                       help='Output directory for charts (default: ./output)')
    parser.add_argument('--find-best', '-b', action='store_true',
                       help='Find and rank the best strategy')
    parser.add_argument('--no-charts', action='store_true',
                       help='Skip generating charts')
    parser.add_argument('--weight-balance', type=float, default=0.40,
                       help='Weight for final balance in scoring (default: 0.40)')
    parser.add_argument('--weight-gains', type=float, default=0.25,
                       help='Weight for total gains in scoring (default: 0.25)')
    parser.add_argument('--weight-tax-adv', type=float, default=0.20,
                       help='Weight for tax-advantaged ratio in scoring (default: 0.20)')
    parser.add_argument('--weight-roi', type=float, default=0.15,
                       help='Weight for ROI in scoring (default: 0.15)')
    
    args = parser.parse_args()
    
    # Create output directory
    output_dir = Path(args.output)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Initialize analyzer
    analyzer = FinanceAnalyzer()
    
    # Load all files
    print("\nLoading simulations...")
    for filepath in args.files:
        # Handle wildcards
        if '*' in filepath:
            analyzer.load_multiple(filepath)
        else:
            analyzer.load_simulation(filepath)
    
    if not analyzer.simulations:
        print("Error: No simulations were loaded!")
        return
    
    print(f"\nTotal simulations loaded: {len(analyzer.simulations)}")
    
    # Find best strategy if requested
    if args.find_best:
        weights = {
            'final_balance': args.weight_balance,
            'total_gains': args.weight_gains,
            'tax_advantaged_ratio': args.weight_tax_adv,
            'roi': args.weight_roi
        }
        
        # Normalize weights
        total_weight = sum(weights.values())
        weights = {k: v/total_weight for k, v in weights.items()}
        
        print(f"\nUsing scoring weights: {weights}")
        
        ranking_df = analyzer.find_best_strategy(weights=weights, verbose=True)
        
        # Save ranking to CSV
        csv_path = output_dir / 'strategy_ranking.csv'
        ranking_df.to_csv(csv_path)
        print(f"\n✓ Saved ranking table to {csv_path}")
    
    # Generate charts
    if not args.no_charts:
        print("\nGenerating visualizations...")
        
        # If comparing multiple strategies
        if len(analyzer.simulations) > 1:
            fig = analyzer.compare_simulations()
            fig_path = output_dir / 'comparison.png'
            plt.savefig(fig_path, dpi=300, bbox_inches='tight')
            print(f"✓ Saved comparison chart to {fig_path}")
            plt.close()
        
        # Individual strategy charts
        for name in analyzer.simulations.keys():
            # Balance over time
            fig = analyzer.plot_account_balances(name)
            fig_path = output_dir / f'{name}_balances.png'
            plt.savefig(fig_path, dpi=300, bbox_inches='tight')
            plt.close()
            
            # Allocation pie
            fig = analyzer.plot_allocation_pie(name)
            fig_path = output_dir / f'{name}_allocation.png'
            plt.savefig(fig_path, dpi=300, bbox_inches='tight')
            plt.close()
        
        print(f"✓ Saved all charts to {output_dir}/")
    
    print("\n✓ Analysis complete!\n")


if __name__ == "__main__":
    main()
